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
    ExecutableCommand,
    cursor::{Hide, MoveTo, Show},
    style::{Color, ResetColor, SetBackgroundColor, SetForegroundColor},
    terminal::{self, EnterAlternateScreen, LeaveAlternateScreen},
};
use regex::Regex;
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

const TEXT_COLOUR_HIGH_PERCENT: f64 = 0.5;
const TEXT_COLOUR_LOW_PERCENT: f64 = 0.2;

type TerminalResult<T> = std::io::Result<T>;

#[derive(Debug, Parser)]
#[command(
    name = "timer",
    version,
    about = "A terminal countdown timer with ASCII art output."
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
    let cli = Cli::parse();
    let duration_input = cli
        .duration
        .as_deref()
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .ok_or_else(|| anyhow!("Please specify a timer duration (try `timer 25m`)."))?;

    let duration = parse_duration(duration_input)?;

    let cancel_flag = Arc::new(AtomicBool::new(false));
    {
        let handler_flag = Arc::clone(&cancel_flag);
        ctrlc::set_handler(move || {
            handler_flag.store(true, Ordering::SeqCst);
        })
        .context("failed to install Ctrl+C handler")?;
    }

    let mut terminal = TerminalRenderer::new().context("failed to prepare the terminal")?;

    run_timer(
        &mut terminal,
        duration,
        cli.message.trim(),
        cli.no_bell,
        cli.font.definition(),
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
        let color = pick_color(ratio);

        let lines = build_display(font, &time_string, message, color);
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

    loop {
        if cancel_flag.load(Ordering::SeqCst) {
            return Ok(());
        }
        thread::sleep(Duration::from_millis(100));
    }
}

fn format_time(total_seconds: u64) -> String {
    let hours = total_seconds / 3600;
    let minutes = (total_seconds / 60) % 60;
    let seconds = total_seconds % 60;

    format!("{hours:02}:{minutes:02}:{seconds:02}")
}

fn pick_color(ratio_remaining: f64) -> Color {
    if ratio_remaining > TEXT_COLOUR_HIGH_PERCENT {
        Color::Green
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
}

impl TerminalRenderer {
    fn new() -> TerminalResult<Self> {
        let mut stdout = stdout();
        stdout.execute(EnterAlternateScreen)?;
        stdout.execute(Hide)?;
        Ok(Self { stdout })
    }

    fn render(
        &mut self,
        lines: &[(String, Option<Color>)],
        background: Option<Color>,
    ) -> TerminalResult<()> {
        let (cols, rows) = terminal::size()?;
        let width = cols.max(1) as usize;
        let mut writer = &self.stdout;

        writer.execute(MoveTo(0, 0))?;

        if let Some(bg) = background {
            writer.execute(SetBackgroundColor(bg))?;
        }

        let total_lines = lines.len() as u16;
        let vertical_padding = rows.saturating_sub(total_lines) / 2;
        let blank_line = " ".repeat(width);

        for _ in 0..vertical_padding {
            writer.execute(ResetColor)?;
            if let Some(bg) = background {
                writer.execute(SetBackgroundColor(bg))?;
            }
            writeln!(writer, "{blank_line}")?;
        }

        for (line, color) in lines {
            let truncated = truncate_to_width(line, width);
            let text_width = UnicodeWidthStr::width(truncated.as_str());
            let horizontal_padding = width.saturating_sub(text_width) / 2;
            let right_padding = width.saturating_sub(horizontal_padding + text_width);

            writer.execute(ResetColor)?;
            if let Some(bg) = background {
                writer.execute(SetBackgroundColor(bg))?;
            }
            write!(writer, "{}", " ".repeat(horizontal_padding))?;

            if let Some(color) = color {
                writer.execute(SetForegroundColor(*color))?;
            }

            write!(writer, "{truncated}")?;
            writer.execute(ResetColor)?;
            if let Some(bg) = background {
                writer.execute(SetBackgroundColor(bg))?;
            }
            writeln!(writer, "{}", " ".repeat(right_padding))?;
        }

        let bottom_padding = rows.saturating_sub(vertical_padding + total_lines);
        for _ in 0..bottom_padding {
            writer.execute(ResetColor)?;
            if let Some(bg) = background {
                writer.execute(SetBackgroundColor(bg))?;
            }
            writeln!(writer, "{blank_line}")?;
        }

        writer.execute(ResetColor)?;
        writer.flush()?;
        Ok(())
    }
}

impl Drop for TerminalRenderer {
    fn drop(&mut self) {
        let _ = self.stdout.execute(ResetColor);
        let _ = self.stdout.execute(Show);
        let _ = self.stdout.execute(LeaveAlternateScreen);
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

const CLASSIC_GLYPHS: [Glyph; 12] = [
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
            "  ___  ", " ( _ ) ", " / _ \\ ", "| (_) |", "| (_) |", " \\___/ ",
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

const HASHY_GLYPHS: [Glyph; 12] = [
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

const SOLID_GLYPHS: [Glyph; 12] = [
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
        ch: ' ',
        lines: &[
            "       ", "       ", "       ", "       ", "       ", "       ", "       ",
        ],
    },
];
