use std::cmp;
use std::io::{Stdout, Write, stdout};
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};
use std::thread;
use std::time::{Duration, Instant};

use anyhow::{Context, Result, anyhow, bail};
use clap::{Parser, ValueEnum};
use crossterm::{
    ExecutableCommand, QueueableCommand,
    cursor::{Hide, MoveTo, Show},
    event::{self, Event, KeyCode, KeyEventKind},
    style::{Color, ResetColor, SetBackgroundColor, SetForegroundColor},
    terminal::{
        self, Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode,
        enable_raw_mode,
    },
};
use regex::Regex;
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

const TEXT_COLOUR_HIGH_PERCENT: f64 = 0.5;
const TEXT_COLOUR_LOW_PERCENT: f64 = 0.2;

type TerminalResult<T> = std::io::Result<T>;

fn parse_color(value: &str) -> std::result::Result<Color, String> {
    let raw = value.trim();
    if raw.is_empty() {
        return Err("colour value cannot be empty".into());
    }

    let normalized = raw.to_ascii_lowercase();
    let named = match normalized.as_str() {
        "pink" => Some(Color::Rgb {
            r: 0xcc,
            g: 0x3d,
            b: 0xd4,
        }),
        "purple" => Some(Color::Rgb {
            r: 0xa6,
            g: 0x6e,
            b: 0xeb,
        }),
        "green" => Some(Color::Rgb {
            r: 0x24,
            g: 0x8c,
            b: 0x5f,
        }),
        "blue" => Some(Color::Rgb {
            r: 0x06,
            g: 0x0e,
            b: 0xa1,
        }),
        "yellow" => Some(Color::Rgb {
            r: 0xd9,
            g: 0xed,
            b: 0x77,
        }),
        "white" => Some(Color::White),
        "black" => Some(Color::Black),
        "default" => Some(Color::Rgb {
            r: 0x78,
            g: 0x5c,
            b: 0x9c,
        }),
        _ => None,
    };

    if let Some(color) = named {
        return Ok(color);
    }

    let hex = raw.trim_start_matches('#');
    if hex.len() != 6 || !hex.chars().all(|c| c.is_ascii_hexdigit()) {
        return Err(format!(
            "Invalid colour '{raw}'. Use pink/cyan/green/blue/yellow/white/black or a #RRGGBB hex code."
        ));
    }

    let r = u8::from_str_radix(&hex[0..2], 16).map_err(|_| "invalid red component")?;
    let g = u8::from_str_radix(&hex[2..4], 16).map_err(|_| "invalid green component")?;
    let b = u8::from_str_radix(&hex[4..6], 16).map_err(|_| "invalid blue component")?;

    Ok(Color::Rgb { r, g, b })
}

#[derive(Debug, Parser)]
#[command(
    name = "timerr",
    version,
    about = "A terminal countdown timer with ASCII art output.",
    after_help = "Commands:\n  stopwatch    Run interactive stopwatch mode"
)]
struct Cli {
    /// Duration of the timer. Syntax: __h__m__s (e.g. 25m, 1h15m, 5m30s)
    duration: Option<String>,

    /// Message displayed under the timer
    #[arg(short = 'm', long = "message", default_value = "")]
    message: String,

    /// Do not ring the terminal bell when the timer finishes
    #[arg(long = "no-bell", default_value_t = false)]
    no_bell: bool,

    /// Select the ASCII art font
    #[arg(
        short = 'f',
        long = "font",
        value_enum,
        env = "TIMER_FONT",
        default_value_t = FontChoice::Solid
    )]
    font: FontChoice,

    /// Colour used during the first phase of the countdown
    #[arg(
        long = "color",
        default_value = "#785c9c",
        value_parser = parse_color,
        help = "Colour for the initial countdown phase (pink|purple|green|blue|yellow|white|black|#RRGGBB)"
    )]
    color: Color,

    /// Exit automatically when timer reaches zero (useful for scripts)
    #[arg(long = "auto-end", default_value_t = false)]
    auto_end: bool,
}

#[derive(Debug, Parser)]
#[command(name = "timerr stopwatch", disable_help_subcommand = true)]
struct StopwatchCli {
    /// Select the ASCII art font
    #[arg(
        short = 'f',
        long = "font",
        value_enum,
        env = "TIMER_FONT",
        default_value_t = FontChoice::Solid
    )]
    font: FontChoice,

    /// Stopwatch text colour
    #[arg(
        long = "color",
        default_value = "#785c9c",
        value_parser = parse_color,
        help = "Colour for stopwatch text (pink|purple|green|blue|yellow|white|black|#RRGGBB)"
    )]
    color: Color,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum FontChoice {
    Classic,
    Hashy,
    Solid,
}

impl FontChoice {
    fn definition(self) -> &'static FontDefinition {
        match self {
            FontChoice::Classic => &CLASSIC_FONT,
            FontChoice::Hashy => &HASHY_FONT,
            FontChoice::Solid => &SOLID_FONT,
        }
    }
}

fn main() -> Result<()> {
    let cancel_flag = Arc::new(AtomicBool::new(false));
    {
        let handler_flag = Arc::clone(&cancel_flag);
        ctrlc::set_handler(move || {
            handler_flag.store(true, Ordering::SeqCst);
        })
        .context("failed to install Ctrl+C handler")?;
    }

    let raw_args = std::env::args().collect::<Vec<_>>();
    if raw_args.get(1).map(String::as_str) == Some("stopwatch") {
        let stopwatch = StopwatchCli::parse_from(
            std::iter::once(raw_args[0].clone())
                .chain(raw_args.into_iter().skip(2))
                .collect::<Vec<_>>(),
        );
        let mut terminal = TerminalRenderer::new().context("failed to prepare the terminal")?;
        let summary = run_stopwatch(
            &mut terminal,
            stopwatch.font.definition(),
            stopwatch.color,
            cancel_flag,
        )?;
        drop(terminal);
        print_stopwatch_summary(&summary);
        return Ok(());
    }

    let cli = Cli::parse();
    let duration_input = cli
        .duration
        .as_deref()
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .ok_or_else(|| anyhow!("Please specify a timer duration (try `timerr 25m`)."))?;
    let duration = parse_duration(duration_input)?;

    let mut terminal = TerminalRenderer::new().context("failed to prepare the terminal")?;
    run_timer(
        &mut terminal,
        duration,
        cli.message.trim(),
        cli.no_bell,
        cli.font.definition(),
        cli.color,
        cli.auto_end,
        cancel_flag,
    )?;

    Ok(())
}

fn parse_duration(input: &str) -> Result<Duration> {
    let regex = Regex::new(r"^(?:(\d{1,2})h)?(?:(\d{1,2})m)?(?:(\d{1,2})s)?$")
        .expect("static regex should compile");

    let captures = regex.captures(input).ok_or_else(|| {
        anyhow!(
            "Invalid duration string: {input}\nUse the format __h__m__s (e.g. 25m, 1h30m, 90s)."
        )
    })?;

    let parse_piece = |idx| -> Result<u64> {
        Ok(captures
            .get(idx)
            .map(|m| {
                m.as_str()
                    .trim_end_matches(|c| c == 'h' || c == 'm' || c == 's')
            })
            .filter(|s| !s.is_empty())
            .map(|s| s.parse::<u64>())
            .transpose()?
            .unwrap_or(0))
    };

    let hours = parse_piece(1)?;
    let minutes = parse_piece(2)?;
    let seconds = parse_piece(3)?;

    if hours == 0 && minutes == 0 && seconds == 0 {
        bail!("The timer duration cannot be zero.");
    }

    let total_seconds = hours * 3600 + minutes * 60 + seconds;
    Ok(Duration::from_secs(total_seconds))
}

fn run_timer(
    terminal: &mut TerminalRenderer,
    duration: Duration,
    message: &str,
    no_bell: bool,
    font: &FontDefinition,
    color: Color,
    auto_end: bool,
    cancel_flag: Arc<AtomicBool>,
) -> Result<()> {
    let total_secs = cmp::max(duration.as_secs(), 1);
    let start = Instant::now();
    let mut last_rendered = None;

    loop {
        if cancel_flag.load(Ordering::SeqCst) {
            return Ok(());
        }

        let elapsed = start.elapsed();

        if elapsed >= duration {
            break;
        }

        let remaining = duration - elapsed;
        let remaining_secs = remaining.as_secs();

        if last_rendered == Some(remaining_secs) {
            thread::sleep(Duration::from_millis(100));
            continue;
        }

        last_rendered = Some(remaining_secs);
        let time_string = format_time(remaining_secs);
        let ratio = remaining_secs as f64 / total_secs as f64;
        let timer_color = pick_color(ratio, color);

        let lines = build_display(font, &time_string, message, timer_color);
        terminal.render(&lines, None)?;

        thread::sleep(Duration::from_millis(150));
    }

    // Final state
    let final_lines = build_display(font, "00:00:00", message, Color::White);
    terminal.render(&final_lines, Some(Color::DarkRed))?;

    if cancel_flag.load(Ordering::SeqCst) {
        return Ok(());
    }

    if !no_bell {
        ring_bell();
    }

    // If auto-end is enabled, exit immediately after showing final state
    if auto_end {
        thread::sleep(Duration::from_millis(500)); // Brief pause to show final state
        return Ok(());
    }

    // Otherwise, wait for user to press Ctrl+C
    loop {
        if cancel_flag.load(Ordering::SeqCst) {
            return Ok(());
        }
        thread::sleep(Duration::from_millis(100));
    }
}

#[derive(Debug, Clone)]
struct LapEntry {
    number: usize,
    elapsed: Duration,
}

#[derive(Debug)]
struct StopwatchSummary {
    elapsed: Duration,
    laps: Vec<LapEntry>,
}

fn run_stopwatch(
    terminal: &mut TerminalRenderer,
    font: &FontDefinition,
    timer_color: Color,
    cancel_flag: Arc<AtomicBool>,
) -> Result<StopwatchSummary> {
    let started_at = Instant::now();
    let mut paused_at: Option<Instant> = None;
    let mut paused_total = Duration::ZERO;
    let mut laps: Vec<LapEntry> = Vec::new();
    let mut scroll_offset = 0usize;
    let mut last_displayed_millis = u128::MAX;
    let mut dirty = true;

    loop {
        if cancel_flag.load(Ordering::SeqCst) {
            break;
        }

        while event::poll(Duration::from_millis(1)).context("failed to poll terminal events")? {
            let key_event = match event::read().context("failed to read terminal event")? {
                Event::Key(key_event) if key_event.kind == KeyEventKind::Press => key_event,
                _ => continue,
            };

            match key_event.code {
                KeyCode::Char('q') | KeyCode::Char('Q') => {
                    let elapsed = current_stopwatch_elapsed(started_at, paused_total, paused_at);
                    return Ok(StopwatchSummary { elapsed, laps });
                }
                KeyCode::Esc => {
                    if let Some(pause_start) = paused_at.take() {
                        paused_total += Instant::now().saturating_duration_since(pause_start);
                    } else {
                        paused_at = Some(Instant::now());
                    }
                    dirty = true;
                }
                KeyCode::Char(' ') => {
                    if paused_at.is_none() {
                        let elapsed =
                            current_stopwatch_elapsed(started_at, paused_total, paused_at);
                        laps.push(LapEntry {
                            number: laps.len() + 1,
                            elapsed,
                        });
                        scroll_offset = 0;
                        dirty = true;
                    }
                }
                KeyCode::Down => {
                    scroll_offset = scroll_offset.saturating_add(1);
                    dirty = true;
                }
                KeyCode::PageDown => {
                    scroll_offset = scroll_offset.saturating_add(5);
                    dirty = true;
                }
                KeyCode::Up => {
                    scroll_offset = scroll_offset.saturating_sub(1);
                    dirty = true;
                }
                KeyCode::PageUp => {
                    scroll_offset = scroll_offset.saturating_sub(5);
                    dirty = true;
                }
                KeyCode::Home => {
                    scroll_offset = 0;
                    dirty = true;
                }
                _ => {}
            }
        }

        let elapsed = current_stopwatch_elapsed(started_at, paused_total, paused_at);
        let elapsed_millis = elapsed.as_millis();
        if paused_at.is_some() && elapsed_millis == last_displayed_millis && !dirty {
            thread::sleep(Duration::from_millis(30));
            continue;
        }

        let available_laps = visible_lap_count(font.height, !laps.is_empty());
        let max_offset = laps.len().saturating_sub(available_laps);
        scroll_offset = cmp::min(scroll_offset, max_offset);
        let lines = build_stopwatch_display_with_scroll(
            font,
            elapsed,
            timer_color,
            &laps,
            paused_at.is_some(),
            scroll_offset,
            available_laps,
        );
        terminal.render(&lines, None)?;
        last_displayed_millis = elapsed_millis;
        dirty = false;

        thread::sleep(Duration::from_millis(16));
    }

    let elapsed = current_stopwatch_elapsed(started_at, paused_total, paused_at);
    Ok(StopwatchSummary { elapsed, laps })
}

fn current_stopwatch_elapsed(
    started_at: Instant,
    paused_total: Duration,
    paused_at: Option<Instant>,
) -> Duration {
    let now = paused_at.unwrap_or_else(Instant::now);
    now.saturating_duration_since(started_at)
        .saturating_sub(paused_total)
}

fn visible_lap_count(font_height: usize, has_laps: bool) -> usize {
    let rows = terminal::size()
        .map(|(_, rows)| rows as usize)
        .unwrap_or(24);
    let fixed_lines = font_height + 2 + usize::from(has_laps); // blank line + controls + optional spacer before controls
    rows.saturating_sub(fixed_lines)
}

fn build_stopwatch_display_with_scroll(
    font: &FontDefinition,
    elapsed: Duration,
    timer_color: Color,
    laps: &[LapEntry],
    paused: bool,
    scroll_offset: usize,
    visible_laps: usize,
) -> Vec<(String, Option<Color>)> {
    let stopwatch_text = format_stopwatch_time(elapsed);
    let mut lines: Vec<(String, Option<Color>)> = font
        .render(&stopwatch_text)
        .into_iter()
        .map(|line| (line, Some(timer_color)))
        .collect();

    lines.push((String::new(), None));

    let newest_first = laps.iter().rev().collect::<Vec<_>>();
    let start = cmp::min(scroll_offset, newest_first.len());
    let end = cmp::min(start + visible_laps, newest_first.len());
    for lap in newest_first[start..end].iter() {
        lines.push((
            format!("Lap {}  {}", lap.number, format_stopwatch_time(lap.elapsed)),
            Some(Color::White),
        ));
    }

    if !newest_first.is_empty() {
        lines.push((String::new(), None));
    }

    let controls = if paused {
        "SPACE: lap   ESC: resume   q: quit"
    } else {
        "SPACE: lap   ESC: pause   q: quit"
    };
    lines.push((controls.to_string(), Some(Color::DarkGrey)));

    lines
}

fn print_stopwatch_summary(summary: &StopwatchSummary) {
    println!(
        "Stopwatch elapsed: {}",
        format_stopwatch_time(summary.elapsed)
    );
    if summary.laps.is_empty() {
        println!("No laps recorded.");
        return;
    }

    println!("Lap summary:");
    for lap in &summary.laps {
        println!("Lap {}  {}", lap.number, format_stopwatch_time(lap.elapsed));
    }
}

fn format_time(total_seconds: u64) -> String {
    let hours = total_seconds / 3600;
    let minutes = (total_seconds / 60) % 60;
    let seconds = total_seconds % 60;

    format!("{hours:02}:{minutes:02}:{seconds:02}")
}

fn format_stopwatch_time(duration: Duration) -> String {
    let total_millis = duration.as_millis() as u64;
    let hours = total_millis / 3_600_000;
    let minutes = (total_millis / 60_000) % 60;
    let seconds = (total_millis / 1_000) % 60;
    let millis = total_millis % 1_000;

    format!("{hours:02}:{minutes:02}:{seconds:02}.{millis:03}")
}

fn pick_color(ratio_remaining: f64, base_color: Color) -> Color {
    if ratio_remaining > TEXT_COLOUR_HIGH_PERCENT {
        base_color
    } else if ratio_remaining > TEXT_COLOUR_LOW_PERCENT {
        Color::Yellow
    } else {
        Color::Red
    }
}

fn build_display(
    font: &FontDefinition,
    time_text: &str,
    message: &str,
    timer_color: Color,
) -> Vec<(String, Option<Color>)> {
    let timer_lines = font.render(time_text);
    let mut lines: Vec<(String, Option<Color>)> = timer_lines
        .into_iter()
        .map(|line| (line, Some(timer_color)))
        .collect();

    if !message.trim().is_empty() {
        lines.push(("".into(), None));
        for raw_line in message.lines() {
            let trimmed = raw_line.trim();
            lines.push((trimmed.to_string(), Some(Color::Cyan)));
        }
    }

    lines
}

fn ring_bell() {
    print!("\x07");
    let _ = stdout().flush();
}

struct TerminalRenderer {
    stdout: Stdout,
    last_size: Option<(u16, u16)>,
}

impl TerminalRenderer {
    fn new() -> TerminalResult<Self> {
        let mut stdout = stdout();
        enable_raw_mode()?;
        stdout.execute(EnterAlternateScreen)?;
        stdout.execute(Hide)?;
        Ok(Self {
            stdout,
            last_size: None,
        })
    }

    fn render(
        &mut self,
        lines: &[(String, Option<Color>)],
        background: Option<Color>,
    ) -> TerminalResult<()> {
        let (cols, rows) = terminal::size()?;
        let current_size = (cols, rows);
        let width = cols.max(1) as usize;
        let mut writer = &self.stdout;

        // Only clear if terminal was resized or first render
        let needs_clear = self.last_size != Some(current_size);
        if needs_clear {
            writer.queue(Clear(ClearType::All))?;
        }
        self.last_size = Some(current_size);

        let blank_line = " ".repeat(width);

        for row in 0..rows {
            writer.queue(MoveTo(0, row))?;
            writer.queue(ResetColor)?;
            if let Some(bg) = background {
                writer.queue(SetBackgroundColor(bg))?;
            }
            write!(writer, "{blank_line}")?;
        }

        let total_lines = lines.len() as u16;
        let start_row = rows.saturating_sub(total_lines) / 2;

        for (idx, (line, color)) in lines.iter().enumerate() {
            let truncated = truncate_to_width(line, width);
            let text_width = UnicodeWidthStr::width(truncated.as_str());
            let left_padding = width.saturating_sub(text_width) / 2;
            let right_padding = width.saturating_sub(left_padding + text_width);
            let target_row = start_row + idx as u16;

            writer.queue(MoveTo(0, target_row))?;
            writer.queue(ResetColor)?;
            if let Some(bg) = background {
                writer.queue(SetBackgroundColor(bg))?;
            }
            if let Some(fg) = color {
                writer.queue(SetForegroundColor(*fg))?;
            }

            write!(
                writer,
                "{}{}{}",
                " ".repeat(left_padding),
                truncated,
                " ".repeat(right_padding)
            )?;
        }

        writer.queue(ResetColor)?;
        writer.flush()?;
        Ok(())
    }
}

impl Drop for TerminalRenderer {
    fn drop(&mut self) {
        let _ = self.stdout.execute(ResetColor);
        let _ = self.stdout.execute(Show);
        let _ = self.stdout.execute(LeaveAlternateScreen);
        let _ = disable_raw_mode();
    }
}

struct FontDefinition {
    height: usize,
    spacing: usize,
    glyph_width: usize,
    glyphs: &'static [Glyph],
}

impl FontDefinition {
    fn render(&self, text: &str) -> Vec<String> {
        if text.is_empty() {
            return vec![String::new(); self.height];
        }

        let mut lines = vec![String::new(); self.height];
        let chars: Vec<char> = text.chars().collect();
        for (idx, ch) in chars.iter().enumerate() {
            let glyph_lines = self.find_lines(*ch);

            for (line_idx, glyph_line) in glyph_lines.iter().enumerate() {
                let padded = pad_to_width(glyph_line, self.glyph_width);
                lines[line_idx].push_str(&padded);
            }

            if idx + 1 != chars.len() {
                for line in lines.iter_mut() {
                    line.push_str(&" ".repeat(self.spacing));
                }
            }
        }

        lines
    }

    fn find_lines(&self, ch: char) -> &'static [&'static str] {
        self.glyphs
            .iter()
            .find(|glyph| glyph.ch == ch)
            .map(|glyph| glyph.lines)
            .or_else(|| {
                self.glyphs
                    .iter()
                    .find(|glyph| glyph.ch == ' ')
                    .map(|glyph| glyph.lines)
            })
            .expect("fonts must contain a space glyph")
    }
}

fn truncate_to_width(input: &str, width: usize) -> String {
    if width == 0 {
        return String::new();
    }

    let mut current = 0;
    let mut output = String::new();
    for ch in input.chars() {
        let ch_width = UnicodeWidthChar::width(ch).unwrap_or(0);
        if current + ch_width > width {
            break;
        }
        output.push(ch);
        current += ch_width;
    }

    output
}

fn pad_to_width(input: &str, width: usize) -> String {
    let mut truncated = truncate_to_width(input, width);
    let current_width = UnicodeWidthStr::width(truncated.as_str());
    if current_width < width {
        truncated.push_str(&" ".repeat(width - current_width));
    }
    truncated
}

struct Glyph {
    ch: char,
    lines: &'static [&'static str],
}

const CLASSIC_FONT: FontDefinition = FontDefinition {
    height: 6,
    spacing: 2,
    glyph_width: 7,
    glyphs: &CLASSIC_GLYPHS,
};

const CLASSIC_GLYPHS: [Glyph; 13] = [
    Glyph {
        ch: '0',
        lines: &[
            "  ___  ", " / _ \\ ", "| | | |", "| | | |", "| |_| |", " \\___/ ",
        ],
    },
    Glyph {
        ch: '1',
        lines: &[" __ ", "/_ |", " | |", " | |", " | |", " |_|"],
    },
    Glyph {
        ch: '2',
        lines: &[" ___  ", "|__ \\ ", "   ) |", "  / / ", " / /_ ", "|____|"],
    },
    Glyph {
        ch: '3',
        lines: &[
            " ____  ", "|___ \\ ", "  __) |", " |__ < ", " ___) |", "|____/ ",
        ],
    },
    Glyph {
        ch: '4',
        lines: &[
            " _  _   ", "| || |  ", "| || |_ ", "|__   _|", "   | |  ", "   |_|  ",
        ],
    },
    Glyph {
        ch: '5',
        lines: &[
            " _____ ", "| ____|", "| |__  ", "|___ \\ ", " ___) |", "|____/ ",
        ],
    },
    Glyph {
        ch: '6',
        lines: &[
            "   __  ", "  / /  ", " / /_  ", "| '_ \\ ", "| (_) |", " \\___/ ",
        ],
    },
    Glyph {
        ch: '7',
        lines: &[
            " ______", "|____  |", "    / /", "   / / ", "  / /  ", " /_/   ",
        ],
    },
    Glyph {
        ch: '8',
        lines: &[
            "  ___  ", " / _ \\ ", "| (_) |", "|  _  |", "| (_) |", " \\___/ ",
        ],
    },
    Glyph {
        ch: '9',
        lines: &[
            "  ___  ", " / _ \\ ", "| (_) |", " \\__, |", "   / / ", "  /_/  ",
        ],
    },
    Glyph {
        ch: ':',
        lines: &["   ", " _ ", "(_)", " _ ", "(_)", "   "],
    },
    Glyph {
        ch: '.',
        lines: &["   ", "   ", "   ", "   ", " _ ", "(_)"],
    },
    Glyph {
        ch: ' ',
        lines: &["  ", "  ", "  ", "  ", "  ", "  "],
    },
];

const HASHY_FONT: FontDefinition = FontDefinition {
    height: 6,
    spacing: 1,
    glyph_width: 8,
    glyphs: &HASHY_GLYPHS,
};

const HASHY_GLYPHS: [Glyph; 13] = [
    Glyph {
        ch: '0',
        lines: &[
            " ###### ", "##    ##", "##    ##", "##    ##", "##    ##", " ###### ",
        ],
    },
    Glyph {
        ch: '1',
        lines: &[
            "   ##   ", " ####   ", "   ##   ", "   ##   ", "   ##   ", " ###### ",
        ],
    },
    Glyph {
        ch: '2',
        lines: &[
            " ###### ", "##    ##", "     ## ", "   ##   ", " ##     ", "########",
        ],
    },
    Glyph {
        ch: '3',
        lines: &[
            " ###### ", "##    ##", "    ### ", "      ##", "##    ##", " ###### ",
        ],
    },
    Glyph {
        ch: '4',
        lines: &[
            "##   ##", "##   ##", "##   ##", "########", "     ##", "     ##",
        ],
    },
    Glyph {
        ch: '5',
        lines: &[
            "########", "##      ", "####### ", "      ##", "##    ##", " ###### ",
        ],
    },
    Glyph {
        ch: '6',
        lines: &[
            "  ##### ", " ##     ", "######  ", "##   ## ", "##   ## ", " #####  ",
        ],
    },
    Glyph {
        ch: '7',
        lines: &[
            "########", "     ## ", "    ##  ", "   ##   ", "  ##    ", " ##     ",
        ],
    },
    Glyph {
        ch: '8',
        lines: &[
            " ###### ", "##    ##", " ###### ", "##    ##", "##    ##", " ###### ",
        ],
    },
    Glyph {
        ch: '9',
        lines: &[
            " ###### ", "##    ##", "##    ##", " #######", "      ##", " #####  ",
        ],
    },
    Glyph {
        ch: ':',
        lines: &["    ", " ## ", " ## ", "    ", " ## ", " ## "],
    },
    Glyph {
        ch: '.',
        lines: &["    ", "    ", "    ", "    ", " ## ", " ## "],
    },
    Glyph {
        ch: ' ',
        lines: &["  ", "  ", "  ", "  ", "  ", "  "],
    },
];

const SOLID_FONT: FontDefinition = FontDefinition {
    height: 7,
    spacing: 1,
    glyph_width: 7,
    glyphs: &SOLID_GLYPHS,
};

const SOLID_GLYPHS: [Glyph; 13] = [
    Glyph {
        ch: '0',
        lines: &[
            " █████ ",
            "██   ██",
            "██   ██",
            "██   ██",
            "██   ██",
            "██   ██",
            " █████ ",
        ],
    },
    Glyph {
        ch: '1',
        lines: &[
            "   ██  ",
            " ████  ",
            "   ██  ",
            "   ██  ",
            "   ██  ",
            "   ██  ",
            " █████ ",
        ],
    },
    Glyph {
        ch: '2',
        lines: &[
            " █████ ",
            "██   ██",
            "    ██ ",
            "  ███  ",
            " ██    ",
            "██     ",
            "███████",
        ],
    },
    Glyph {
        ch: '3',
        lines: &[
            " █████ ",
            "██   ██",
            "    ██ ",
            "  ███  ",
            "    ██ ",
            "██   ██",
            " █████ ",
        ],
    },
    Glyph {
        ch: '4',
        lines: &[
            "██   ██",
            "██   ██",
            "██   ██",
            "███████",
            "     ██",
            "     ██",
            "     ██",
        ],
    },
    Glyph {
        ch: '5',
        lines: &[
            "███████",
            "██     ",
            "██████ ",
            "     ██",
            "     ██",
            "██   ██",
            " █████ ",
        ],
    },
    Glyph {
        ch: '6',
        lines: &[
            "  ████ ",
            " ██    ",
            "██     ",
            "██████ ",
            "██   ██",
            "██   ██",
            " █████ ",
        ],
    },
    Glyph {
        ch: '7',
        lines: &[
            "███████",
            "     ██",
            "    ██ ",
            "   ██  ",
            "  ██   ",
            " ██    ",
            " ██    ",
        ],
    },
    Glyph {
        ch: '8',
        lines: &[
            " █████ ",
            "██   ██",
            "██   ██",
            " █████ ",
            "██   ██",
            "██   ██",
            " █████ ",
        ],
    },
    Glyph {
        ch: '9',
        lines: &[
            " █████ ",
            "██   ██",
            "██   ██",
            " ██████",
            "     ██",
            "    ██ ",
            " ████  ",
        ],
    },
    Glyph {
        ch: ':',
        lines: &["   ", " ██", " ██", "   ", " ██", " ██", "   "],
    },
    Glyph {
        ch: '.',
        lines: &["   ", "   ", "   ", "   ", "   ", " ██", " ██"],
    },
    Glyph {
        ch: ' ',
        lines: &[
            "       ", "       ", "       ", "       ", "       ", "       ", "       ",
        ],
    },
];
