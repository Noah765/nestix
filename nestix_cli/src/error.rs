use std::{
    error,
    fmt::{self, Display},
    io,
};

use rnix::ParseError;

#[derive(Debug)]
pub enum Error {
    Walkdir(walkdir::Error),
    Io(io::Error),
    Parse(ParseError),
    Check,
}

impl From<walkdir::Error> for Error {
    fn from(value: walkdir::Error) -> Self {
        Self::Walkdir(value)
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<ParseError> for Error {
    fn from(value: ParseError) -> Self {
        Self::Parse(value)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Walkdir(x) => write!(f, "Directory walker error: {x}"),
            Error::Io(x) => write!(f, "IO error: {x}"),
            Error::Parse(x) => write!(f, "Parse error: {x}"),
            Error::Check => Ok(()),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Error::Walkdir(x) => Some(x),
            Error::Io(x) => Some(x),
            Error::Parse(x) => Some(x),
            Error::Check => None,
        }
    }
}
