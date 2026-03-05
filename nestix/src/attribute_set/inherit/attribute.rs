use crate::{
    attribute_set::inherit::format::InheritFormat, comment::Comment, formatter::Formatter,
};

/// A Nix inherit attribute.
///
/// `InheritAttribute` supports comments above and to the right of the attribute
/// name, for example:
/// ```nix
/// {
///   inherit
///     (from)
///     # Above attr1
///     attr1 # Right of attr1
///     ;
/// }
/// ```
///
/// In inline inherit nodes, comments between the from segment and the first
/// attribute are interpreted as being above the first attribute. Comments after
/// the last attribute and between attributes are interpreted as being to the
/// right of the previous attribute. For example:
/// ```nix
/// {
///   inherit (from) /* Above attr1 */ attr1 /* Right of attr1 */ attr2 /* Right of attr2 */;
/// }
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InheritAttribute {
    comments_above: Vec<Comment>,
    comments_right: Vec<Comment>,
    attribute: String,
}

impl InheritAttribute {
    /// Constructs a new `InheritAttribute` from `comments_above`,
    /// `comments_right` and `attribute`.
    pub fn new(
        comments_above: Vec<Comment>,
        comments_right: Vec<Comment>,
        attribute: String,
    ) -> Self {
        Self {
            comments_above,
            comments_right,
            attribute,
        }
    }

    /// Writes this inherit attribute to `formatter` using `format`.
    pub fn print(&self, formatter: &mut Formatter, format: &InheritFormat) {
        let separator = match format {
            InheritFormat::Inline => " ",
            InheritFormat::Multiline { .. } => &formatter.line_separator(),
        };
        for x in &self.comments_above {
            x.print(formatter);
            formatter.write(separator);
        }

        formatter.write(&self.attribute);

        for x in &self.comments_right {
            formatter.write(" ");
            x.print(formatter);
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::formatter::IndentationType;

    use super::*;

    #[test]
    fn print_inline() {
        let mut formatter = Formatter::new(IndentationType::TwoSpaces);
        InheritAttribute::new(
            vec![Comment::new("/* above */")],
            vec![Comment::new("/* right */")],
            String::from("attr1"),
        )
        .print(&mut formatter, &InheritFormat::Inline);
        assert_eq!(formatter.into_string(), "/* above */ attr1 /* right */");
    }

    #[test]
    fn print_multiline() {
        let mut formatter = Formatter::new(IndentationType::TwoSpaces);
        formatter.increase_indentation();
        let format = InheritFormat::Multiline {
            from_on_separate_line: false,
        };
        InheritAttribute::new(
            vec![Comment::new("/* above */")],
            vec![Comment::new("/* right */")],
            String::from("attr1"),
        )
        .print(&mut formatter, &format);
        assert_eq!(formatter.into_string(), "/* above */\n  attr1 /* right */");
    }
}
