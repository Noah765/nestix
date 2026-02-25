use std::iter;

use crate::formatter::Formatter;

/// A Nix comment.
//
// INVARIANT: !0.is_empty()
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comment(Vec<String>);

impl Comment {
    /// Constructs a new `Comment` based on the comment string `text`.
    ///
    /// This method does not check whether the provided `text` is actually a
    /// valid Nix comment string.
    pub fn new(text: &str) -> Self {
        // `lines` is not empty, even if `text` is empty.
        let lines: Vec<_> = text.trim_start().lines().collect();
        if lines.len() == 1 {
            return Self(vec![String::from(lines[0])]);
        }

        let indentation = lines[1..lines.len() - 1]
            .into_iter()
            .map(|x| &x[..x.len() - x.trim_start().len()])
            .min_by_key(|x| x.len())
            .map_or_else(String::new, |x| String::from(x));

        let last_line = lines
            .last()
            .expect("`lines` should not be empty")
            .trim_start()
            .to_string();
        let lines = lines[..lines.len() - 1]
            .into_iter()
            .map(|x| x.strip_prefix(&indentation).unwrap_or(x).to_string())
            .chain(iter::once(last_line))
            .collect();

        Self(lines)
    }

    /// Writes this comment to `formatter`.
    pub fn print(&self, formatter: &mut Formatter) {
        formatter.write(&self.0[0]);

        for x in &self.0[1..] {
            formatter.open_line();
            formatter.write(x);
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::formatter::IndentationType;

    use super::*;

    #[test]
    fn new_single_line() {
        assert_eq!(
            Comment::new("  # Single line comment  "),
            Comment(vec![String::from("# Single line comment  ")])
        );
    }

    #[test]
    fn new_multi_line() {
        assert_eq!(
            Comment::new(
                "  /* After start  \n  \tSecond line  \n    Third line\n  \t  Fourth line \n     Before end */  "
            ),
            Comment(vec![
                String::from("/* After start  "),
                String::from("Second line  "),
                String::from("    Third line"),
                String::from("  Fourth line "),
                String::from("Before end */  "),
            ])
        );
    }

    #[test]
    fn print_single_line() {
        let mut formatter = Formatter::new(IndentationType::TwoSpaces);
        Comment::new("# single").print(&mut formatter);
        assert_eq!(formatter.into_string(), "# single");
    }

    #[test]
    fn print_multi_line() {
        let mut formatter = Formatter::new(IndentationType::TwoSpaces);
        formatter.increase_indentation();
        Comment::new("/*\n  first\nsecond\n*/").print(&mut formatter);
        assert_eq!(formatter.into_string(), "/*\n    first\n  second\n  */");
    }
}
