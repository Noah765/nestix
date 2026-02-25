use crate::{
    attribute_set::inherit::format::InheritFormat, comment::Comment, formatter::Formatter,
    parser::Parser,
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
    /// Constructs a new `InheritAttribute` by consuming elements using `parser`.
    ///
    /// # Panics
    ///
    /// Panics if `parser` is not in front of comments preceding this attribute
    /// or this attribute itself.
    pub fn new(parser: &mut Parser) -> Self {
        let comments_above = parser.next_comments();
        let attribute = parser.next_attribute().to_string();
        let comments_right = parser.next_comment_line();

        parser.skip_whitespace();

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
    use rnix::{
        Root, SyntaxKind,
        ast::{Expr, HasEntry},
    };
    use rowan::ast::AstNode;

    use crate::formatter::IndentationType;

    use super::*;

    fn parse_string_to_inherit_attributes(set: &str) -> Vec<InheritAttribute> {
        let mut parser = match Root::parse(set).ok().unwrap().expr().unwrap() {
            Expr::AttrSet(x) => Parser::new(x.inherits().next().unwrap().syntax().clone()),
            _ => panic!(),
        };
        parser.skip_after(|x| x.kind() == SyntaxKind::NODE_INHERIT_FROM);

        let mut attributes = Vec::new();
        while let Some(x) = parser.peek()
            && x.kind() != SyntaxKind::TOKEN_SEMICOLON
        {
            attributes.push(InheritAttribute::new(&mut parser));
        }
        attributes
    }

    #[test]
    fn new() {
        let set = "{inherit (from)\n# above \n${/**/ attr1}\t /* attr1 */\n/* attr2 */\"attr2\"attr3 /* attr3 */;}";
        assert_eq!(
            parse_string_to_inherit_attributes(set),
            [
                InheritAttribute {
                    comments_above: vec![Comment::new("# above ")],
                    comments_right: vec![Comment::new("/* attr1 */")],
                    attribute: String::from("${/**/ attr1}"),
                },
                InheritAttribute {
                    comments_above: vec![Comment::new("/* attr2 */")],
                    comments_right: Vec::new(),
                    attribute: String::from("\"attr2\""),
                },
                InheritAttribute {
                    comments_above: Vec::new(),
                    comments_right: vec![Comment::new("/* attr3 */")],
                    attribute: String::from("attr3"),
                }
            ]
        );
    }

    #[test]
    fn print_inline() {
        let set = "{inherit (from) /* above */  attr1\t/* right */;}";
        let mut formatter = Formatter::new(IndentationType::TwoSpaces);
        parse_string_to_inherit_attributes(set)[0].print(&mut formatter, &InheritFormat::Inline);
        assert_eq!(formatter.into_string(), "/* above */ attr1 /* right */");
    }

    #[test]
    fn print_multiline() {
        let set = "{inherit (from)\n/* above */ attr1\t/* right */;}";
        let mut formatter = Formatter::new(IndentationType::TwoSpaces);
        let format = InheritFormat::Multiline {
            from_on_separate_line: false,
        };
        formatter.increase_indentation();
        parse_string_to_inherit_attributes(set)[0].print(&mut formatter, &format);
        assert_eq!(formatter.into_string(), "/* above */\n  attr1 /* right */");
    }
}
