use std::iter;

/// A Nix comment.
#[derive(Debug, PartialEq, Eq)]
// INVARIANT: !0.is_empty()
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
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

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
}
