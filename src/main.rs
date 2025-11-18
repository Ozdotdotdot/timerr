use std::cmp;
use std::io::{Stdout, Write, stdout};
use std::thread;
use std::time::{Duration, Instant};

use anyhow::{Context, Result, anyhow, bail};
use clap::{Parser, ValueEnum};
use crossterm::{
    ExecutableCommand,
    cursor::{Hide, MoveTo, Show},
    style::{Color, ResetColor, SetForegroundColor},
    terminal::{self, Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen},
};
use regex::Regex;

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
        default_value_t = FontChoice::Classic
    )]
    font: FontChoice,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum FontChoice {
    Classic,
    Blocky,
}

impl FontChoice {
    fn definition(self) -> &'static FontDefinition {
        match self {
            FontChoice::Classic => &CLASSIC_FONT,
            FontChoice::Blocky => &BLOCKY_FONT,
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

    let mut terminal = TerminalRenderer::new().context("failed to prepare the terminal")?;

    run_timer(
        &mut terminal,
        duration,
        cli.message.trim(),
        cli.no_bell,
        cli.font.definition(),
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
) -> Result<()> {
    let total_secs = cmp::max(duration.as_secs(), 1);
    let start = Instant::now();
    let mut last_rendered = None;

    loop {
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
        terminal.render(&lines)?;

        thread::sleep(Duration::from_millis(150));
    }

    // Final state
    let final_lines = build_display(font, "00:00:00", message, Color::DarkRed);
    terminal.render(&final_lines)?;

    loop {
        if !no_bell {
            print!("\x07");
            std::io::stdout().flush().ok();
        }
        thread::sleep(Duration::from_secs(10));
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

    let timer_width = lines.iter().map(|(line, _)| line.len()).max().unwrap_or(0);

    if !message.trim().is_empty() {
        lines.push(("".into(), None));
        for raw_line in message.lines() {
            let trimmed = raw_line.trim_end();
            let padding = timer_width.saturating_sub(trimmed.len()) / 2;
            let centered = format!("{}{}", " ".repeat(padding), trimmed);
            lines.push((centered, Some(Color::Cyan)));
        }
    }

    lines
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

    fn render(&mut self, lines: &[(String, Option<Color>)]) -> TerminalResult<()> {
        let (cols, rows) = terminal::size()?;
        let mut writer = &self.stdout;

        writer.execute(Clear(ClearType::All))?;
        writer.execute(MoveTo(0, 0))?;

        let total_lines = lines.len() as u16;
        let vertical_padding = rows.saturating_sub(total_lines) / 2;

        for _ in 0..vertical_padding {
            writeln!(writer)?;
        }

        for (line, color) in lines {
            let line_width = line.len() as u16;
            let horizontal_padding = if cols > line_width {
                ((cols - line_width) / 2) as usize
            } else {
                0
            };

            if let Some(color) = color {
                writer.execute(SetForegroundColor(*color))?;
            } else {
                writer.execute(ResetColor)?;
            }

            writeln!(writer, "{}{}", " ".repeat(horizontal_padding), line)?;
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
                lines[line_idx].push_str(glyph_line);
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

struct Glyph {
    ch: char,
    lines: &'static [&'static str],
}

const CLASSIC_FONT: FontDefinition = FontDefinition {
    height: 5,
    spacing: 2,
    glyphs: &CLASSIC_GLYPHS,
};

const CLASSIC_GLYPHS: [Glyph; 12] = [
    Glyph {
        ch: '0',
        lines: &["  ___  ", " / _ \\ ", "| | | |", "| |_| |", " \\___/ "],
    },
    Glyph {
        ch: '1',
        lines: &[" __ ", "/_ |", " | |", " | |", " |_|"],
    },
    Glyph {
        ch: '2',
        lines: &[" ___  ", "|__ \\ ", "   ) |", "  / / ", " |____|"],
    },
    Glyph {
        ch: '3',
        lines: &[" ____  ", "|___ \\ ", "  __) |", " |__ < ", "|____/ "],
    },
    Glyph {
        ch: '4',
        lines: &[" _  _   ", "| || |  ", "| || |_ ", "|__   _|", "   |_|  "],
    },
    Glyph {
        ch: '5',
        lines: &[" _____ ", "| ____|", "| |__  ", "|___ \\ ", " ___) |"],
    },
    Glyph {
        ch: '6',
        lines: &["   __  ", "  / /  ", " / /_  ", "| '_ \\ ", "| (_) |"],
    },
    Glyph {
        ch: '7',
        lines: &[" ______", "|____  |", "    / /", "   / / ", "  /_/  "],
    },
    Glyph {
        ch: '8',
        lines: &["  ___  ", " ( _ ) ", " / _ \\ ", "| (_) |", " \\___/ "],
    },
    Glyph {
        ch: '9',
        lines: &["  ___  ", " / _ \\ ", "| (_) |", " \\__, |", "   /_/ "],
    },
    Glyph {
        ch: ':',
        lines: &["   ", " _ ", "(_)", " _ ", "(_)"],
    },
    Glyph {
        ch: ' ',
        lines: &["  ", "  ", "  ", "  ", "  "],
    },
];

const BLOCKY_FONT: FontDefinition = FontDefinition {
    height: 6,
    spacing: 1,
    glyphs: &BLOCKY_GLYPHS,
};

const BLOCKY_GLYPHS: [Glyph; 12] = [
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
