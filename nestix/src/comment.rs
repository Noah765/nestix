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
        let indentation = text
            .trim_start()
            .lines()
            .skip(1)
            .filter(|x| x.trim_start().len() != 0)
            .map(|x| &x[..x.len() - x.trim_start().len()])
            .min_by_key(|x| x.len())
            .unwrap_or("");

        let mut lines: Vec<_> = text
            .trim_start()
            .lines()
            .map(|x| {
                if x.trim_start().len() == 0 {
                    String::from("")
                } else {
                    x.strip_prefix(&indentation).unwrap_or(x).to_string()
                }
            })
            .collect();
        let last = lines.last_mut().expect("`lines` should not be empty");
        last.replace_range(..last.len() - last.trim_start().len(), "");

        Self(lines)
    }

    /// Writes this comment to `formatter`.
    pub fn print(&self, formatter: &mut Formatter) {
        formatter.write(&self.0[0]);

        for x in &self.0[1..] {
            if x.is_empty() {
                formatter.write("\n");
            } else {
                formatter.open_line();
                formatter.write(x);
            }
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
                "  /* After start  \n\t\n  \tSecond line  \n\t   \t \n    Third line\n  \t  Fourth line \n  \tBefore end */  "
            ),
            Comment(vec![
                String::from("/* After start  "),
                String::from(""),
                String::from("Second line  "),
                String::from(""),
                String::from("    Third line"),
                String::from("  Fourth line "),
                String::from("Before end */  "),
            ])
        );
        assert_eq!(
            Comment::new(" /**\n    Line\n  /**"),
            Comment(vec![
                String::from("/**"),
                String::from("  Line"),
                String::from("/**")
            ])
        );
        assert_eq!(
            Comment::new("  /**\nLine\n  /**"),
            Comment(vec![
                String::from("/**"),
                String::from("Line"),
                String::from("/**")
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
        Comment::new("/*\n  first\n\nsecond\n*/").print(&mut formatter);
        assert_eq!(formatter.into_string(), "/*\n    first\n\n  second\n  */");
    }
}
