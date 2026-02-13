use std::{
    collections::HashSet,
    fs,
    io::{self, Read},
    path::PathBuf,
};

use clap::Parser;
use nestix::formatter::Formatter;
use walkdir::{DirEntry, WalkDir};

use crate::{cli::Cli, error::Error};

mod cli;
pub mod error;

pub fn run() -> Result<(), Error> {
    let cli = Cli::parse();

    if cli.include.is_empty() {
        format_stdin(cli)
    } else {
        format_paths(cli)
    }
}

fn format_stdin(cli: Cli) -> Result<(), Error> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let output = Formatter::format(&input)?;

    if cli.check && output != input {
        eprintln!("Stdin requires formatting");
        return Err(Error::Check);
    }

    if !cli.check {
        print!("{output}");
    }

    Ok(())
}

fn format_paths(cli: Cli) -> Result<(), Error> {
    let paths = get_nix_file_paths(
        cli.include.into_iter().collect(),
        cli.exclude.into_iter().collect(),
        cli.follow_symlinks,
    )?;

    let mut counter = 0;
    for x in paths {
        let input = fs::read_to_string(&x)?;
        let output = Formatter::format(&input)?;

        if output == input {
            continue;
        }

        counter += 1;

        if cli.check {
            eprintln!("Requires formatting: {}", x.display());
        } else {
            fs::write(&x, output)?;
            println!("Formatted: {}", x.display());
        }
    }

    if counter == 0 && cli.check {
        println!("All files are formatted.");
    } else if cli.check {
        eprintln!("\n{counter} files require formatting.");
        return Err(Error::Check);
    } else if counter == 0 {
        println!("No files were formatted.");
    } else {
        println!("\n{counter} files were formatted.");
    }

    Ok(())
}

fn get_nix_file_paths(
    mut include: HashSet<PathBuf>,
    exclude: HashSet<PathBuf>,
    follow_symlinks: bool,
) -> walkdir::Result<Vec<PathBuf>> {
    let mut visited_roots = HashSet::new();
    let mut files = Vec::new();

    while let Some(root) = include.iter().next().cloned() {
        include.remove(&root);

        if exclude.iter().any(|x| root.starts_with(x)) {
            continue;
        }

        let iter = WalkDir::new(&root)
            .follow_links(follow_symlinks)
            .into_iter()
            .filter_entry(|x| {
                !visited_roots.contains(x.path())
                    && !exclude.contains(x.path())
                    && (x.file_type().is_dir() || is_nix_file(x))
            });

        for x in iter {
            let path = x?.into_path();
            include.remove(&path);
            if path.is_file() {
                files.push(path);
            }
        }

        visited_roots.insert(root);
    }

    Ok(files)
}

fn is_nix_file(entry: &DirEntry) -> bool {
    entry.file_type().is_file()
        && entry
            .file_name()
            .to_str()
            .is_some_and(|x| x.ends_with(".nix"))
}
