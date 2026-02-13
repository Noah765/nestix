use std::process::ExitCode;

use nestix_cli::error::Error;

fn main() -> ExitCode {
    match nestix_cli::run() {
        Ok(_) => ExitCode::SUCCESS,
        Err(Error::Check) => ExitCode::from(2),
        Err(x) => {
            eprintln!("{x}");
            ExitCode::FAILURE
        }
    }
}
