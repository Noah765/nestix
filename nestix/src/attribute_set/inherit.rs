use rnix::{
    SyntaxKind,
    ast::{self, InheritFrom},
};
use rowan::ast::AstNode;

use crate::{
    attribute_set::inherit::{attribute::InheritAttribute, format::InheritFormat},
    comment::Comment,
    formatter::Formatter,
    parser::Parser,
};

mod attribute;
pub mod format;

/// A Nix inherit node.
///
/// `Inherit` supports comments before and to the right of the from segment
/// when it is present and below the from segment (or after the inherit keyword
/// if the from segment is absent) when no attributes are specified. When
/// attributes are specified, comments below the from segment are interpreted as
/// being above the first attribute.
#[derive(Clone, Debug)]
pub struct Inherit {
    format: InheritFormat,
    comments_before_from: Vec<Comment>,
    comments_right_of_from: Vec<Comment>,
    comments_below_from: Vec<Comment>,
    comments_right: Vec<Comment>,
    from: Option<InheritFrom>,
    attributes: Vec<InheritAttribute>,
}

impl Inherit {
    /// Constructs a new `Inherit` based on `node`.
    pub fn new(node: ast::Inherit, comments_right: Vec<Comment>) -> Self {
        let mut parser = Parser::new(node.syntax().clone());
        parser.next_token();

        let (comments_before_from, comments_right_of_from) = match node.from() {
            None => (Vec::new(), Vec::new()),
            Some(_) => {
                let comments_before_from = parser.next_comments();
                parser.next();
                (comments_before_from, parser.next_comment_line())
            }
        };
        let comments_below_from = match node.attrs().next() {
            None => parser.next_comments(),
            Some(_) => Vec::new(),
        };

        let mut attributes = Vec::new();
        while let Some(x) = parser.peek()
            && x.kind() != SyntaxKind::TOKEN_SEMICOLON
        {
            attributes.push(InheritAttribute::new(&mut parser));
        }

        Self {
            format: InheritFormat::new(node.clone()),
            comments_before_from,
            comments_right_of_from,
            comments_below_from,
            comments_right,
            from: node.from(),
            attributes,
        }
    }

    /// Writes this inherit node to `formatter`.
    pub fn print(&self, formatter: &mut Formatter) {
        formatter.write("inherit");
        formatter.increase_indentation();

        let separator = match self.format {
            InheritFormat::Multiline {
                from_on_separate_line,
            } if from_on_separate_line => &formatter.line_separator(),
            _ => " ",
        };
        for x in &self.comments_before_from {
            formatter.write(separator);
            x.print(formatter);
        }
        if let Some(x) = &self.from {
            formatter.write(separator);
            formatter.format_node(x.syntax().clone());
        }
        for x in &self.comments_right_of_from {
            formatter.write(" ");
            x.print(formatter);
        }
        for x in &self.comments_below_from {
            formatter.open_line();
            x.print(formatter);
        }

        let separator = match self.format {
            InheritFormat::Inline => " ",
            InheritFormat::Multiline { .. } => &formatter.line_separator(),
        };
        for x in &self.attributes {
            formatter.write(separator);
            x.print(formatter, &self.format);
        }

        if let InheritFormat::Multiline { .. } = self.format {
            formatter.write(separator);
        }
        formatter.write(";");

        for x in &self.comments_right {
            formatter.write(" ");
            x.print(formatter);
        }

        formatter.decrease_indentation();
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rnix::{
        Root,
        ast::{Expr, HasEntry},
    };

    use crate::formatter::IndentationType;

    use super::*;

    #[test]
    fn new() {
        fn test(
            set: &str,
            comments_before_from: &[Comment],
            comments_right_of_from: &[Comment],
            comments_below_from: &[Comment],
        ) {
            let inherit = match Root::parse(set).ok().unwrap().expr().unwrap() {
                Expr::AttrSet(x) => Inherit::new(x.inherits().next().unwrap(), Vec::new()),
                _ => panic!(),
            };
            assert_eq!(inherit.comments_before_from, comments_before_from, "{set}");
            assert_eq!(
                inherit.comments_right_of_from, comments_right_of_from,
                "{set}"
            );
            assert_eq!(inherit.comments_below_from, comments_below_from, "{set}");
        }

        test("{inherit (from) attr1;}", &[], &[], &[]);
        test(
            "{inherit /* first */ # second\n(from) /* right of from */ attr1;}",
            &[Comment::new("/* first */"), Comment::new("# second")],
            &[Comment::new("/* right of from */")],
            &[],
        );
        test("{inherit /* above attr1 */ attr1;}", &[], &[], &[]);
        test(
            "{inherit # before\n(from) # right\n/* first below */\n\n/* second below */;}",
            &[Comment::new("# before")],
            &[Comment::new("# right")],
            &[
                Comment::new("/* first below */"),
                Comment::new("/* second below */"),
            ],
        );
        test(
            "{inherit /* below */;}",
            &[],
            &[],
            &[Comment::new("/* below */")],
        );
    }

    #[test]
    fn print() {
        fn test(set: &str, expected: &str) {
            let inherit = match Root::parse(set).ok().unwrap().expr().unwrap() {
                Expr::AttrSet(x) => Inherit::new(x.inherits().next().unwrap(), Vec::new()),
                _ => panic!(),
            };
            let mut formatter = Formatter::new(IndentationType::TwoSpaces);
            inherit.print(&mut formatter);
            assert_eq!(formatter.into_string(), expected, "{set}");
        }

        test("{inherit  ;}", "inherit;");
        test(
            "{inherit (from)\tattr1  attr2;}",
            "inherit (from) attr1 attr2;",
        );
        test(
            "{inherit/* before */(from)/* right */attr1/* attr1 */;}",
            "inherit /* before */ (from) /* right */ attr1 /* attr1 */;",
        );
        test("{inherit\nattr1\nattr2;}", "inherit\n  attr1\n  attr2\n  ;");
        test(
            "{inherit (from)\n/* below */;}",
            "inherit (from)\n  /* below */\n  ;",
        );
        test(
            "{inherit/* before */(from)\nattr1 attr2\n;}",
            "inherit /* before */ (from)\n  attr1\n  attr2\n  ;",
        );
        test(
            "{inherit/* before */\n(from)# right\nattr1 attr2\n;}",
            "inherit\n  /* before */\n  (from) # right\n  attr1\n  attr2\n  ;",
        );
    }
}
