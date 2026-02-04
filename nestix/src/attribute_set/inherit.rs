use rnix::{
    SyntaxKind,
    ast::{self, InheritFrom},
};
use rowan::ast::AstNode;

use crate::{
    attribute_set::inherit::{attribute::InheritAttribute, format::InheritFormat},
    comment::Comment,
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
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rnix::{
        Root,
        ast::{Expr, HasEntry},
    };

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
}
