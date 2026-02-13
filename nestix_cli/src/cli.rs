use std::{fs::canonicalize, path::PathBuf};

use clap::{
    Parser,
    builder::{
        PathBufValueParser, Styles, TypedValueParser,
        styling::{AnsiColor, Color, Style},
    },
};

const YELLOW: Option<Color> = Some(Color::Ansi(AnsiColor::Yellow));
const GREEN: Option<Color> = Some(Color::Ansi(AnsiColor::Green));
const RED: Option<Color> = Some(Color::Ansi(AnsiColor::Red));

const STYLES: Styles = Styles::styled()
    .header(Style::new().fg_color(YELLOW).bold())
    .usage(Style::new().fg_color(YELLOW).bold())
    .literal(Style::new().fg_color(GREEN).bold())
    .placeholder(Style::new().fg_color(GREEN))
    .valid(Style::new().fg_color(GREEN).bold())
    .invalid(Style::new().fg_color(RED).bold())
    .context(Style::new().fg_color(GREEN));

/// A structural Nix code formatter.
#[derive(Debug, Parser)]
#[command(name = "nestix", version, styles = STYLES)]
pub struct Cli {
    /// Paths to format recursively, read from stdin if omitted
    #[arg(value_parser = PathBufValueParser::new().try_map(canonicalize))]
    pub include: Vec<PathBuf>,

    /// Exit with a status code of 2 if formatting changes are needed
    #[arg(short, long)]
    pub check: bool,

    /// Paths to exclude from formatting
    #[arg(short, long, value_name = "PATH", value_parser = PathBufValueParser::new().try_map(canonicalize))]
    pub exclude: Vec<PathBuf>,

    /// Follow symbolic links while traversing directories
    #[arg(short, long)]
    pub follow_symlinks: bool,
}
