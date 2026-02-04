use rnix::ast::{AttrpathValue, Expr};
use rowan::ast::AstNode;

use crate::{attribute_set::format::AttributeSetFormat, comment::Comment, parser::Parser};

/// An Nix attribute set attribute.
///
/// `Attribute` supports comments to the right of the attribute as well as
/// around the equals sign.
/// The value of `Attribute` can be an attribute set (stored as a vector of
/// attribute node indices) or an arbitrary expression.
#[derive(Clone, Debug)]
pub struct Attribute {
    name: String,
    comments_before_equal: Vec<Comment>,
    comments_after_equal: Vec<Comment>,
    comments_right: Vec<Comment>,
    value: AttributeValue,
}

#[derive(Clone, Debug)]
enum AttributeValue {
    // INVARIANT: inline -> !roots.is_empty() && roots does not contain inherits
    AttributeSet {
        inline: bool,
        format: Option<AttributeSetFormat>,
        roots: Vec<usize>,
    },
    Expression(Expr),
}

impl Attribute {
    /// Extends `nodes` by an attribute constructed from `node` and
    /// `comments_right`, as well as the attribute's subtree in the attribute
    /// tree, in that order.
    pub fn construct(
        nodes: &mut Vec<Attribute>,
        node: AttrpathValue,
        comments_right: Vec<Comment>,
    ) {
        let mut parser = Parser::new(node.syntax().clone());
        let mut path_parser = Parser::new(parser.next_attribute_path().syntax().clone());

        while path_parser.peek().is_some() {
            let mut comments_above = path_parser.next_comments();
            let name = path_parser.next_attribute().to_string();
            comments_above.extend(path_parser.next_comments());
            // TODO Use comments_above

            nodes.push(Self {
                name,
                comments_before_equal: Vec::new(),
                comments_after_equal: Vec::new(),
                comments_right: Vec::new(),
                value: AttributeValue::AttributeSet {
                    inline: true,
                    format: None,
                    roots: vec![nodes.len() + 1],
                },
            });

            path_parser.next();
        }

        let last = nodes.last_mut().expect("`nodes` should not be empty");

        last.comments_before_equal = parser.next_comments();
        parser.next();
        last.comments_after_equal = parser.next_comments();

        last.value = AttributeValue::Expression(parser.next_expression()); // TODO Support attribute set values.

        last.comments_right = parser.next_comments();
        last.comments_right.extend(comments_right);
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rnix::{Root, ast::HasEntry};

    use super::*;

    fn parse_string_to_attribute(input: &str, comments_right: Vec<&str>) -> Vec<Attribute> {
        let mut nodes = Vec::new();
        match Root::parse(input).ok().unwrap().expr().unwrap() {
            Expr::AttrSet(x) => Attribute::construct(
                &mut nodes,
                x.attrpath_values().next().unwrap(),
                comments_right.into_iter().map(Comment::new).collect(),
            ),
            _ => panic!(),
        }
        nodes
    }

    #[test]
    fn construct() {
        let attributes = parse_string_to_attribute(
            "{attr1/*before*/./*after*/\"attr2\"/*before next*/.${attr3}.\"attr${\"\"}4\"/*before equal*/=/*after equal*/(true)/*before*/;/*Right*/}",
            vec!["/*Right*/"],
        );

        match &attributes[..] {
            [
                first @ Attribute {
                    value:
                        AttributeValue::AttributeSet {
                            inline: true,
                            format: None,
                            roots: first_roots,
                        },
                    ..
                },
                second @ Attribute {
                    value:
                        AttributeValue::AttributeSet {
                            inline: true,
                            format: None,
                            roots: second_roots,
                        },
                    ..
                },
                third @ Attribute {
                    value:
                        AttributeValue::AttributeSet {
                            inline: true,
                            format: None,
                            roots: third_roots,
                        },
                    ..
                },
                fourth @ Attribute {
                    value: AttributeValue::Expression(value),
                    ..
                },
            ] => {
                assert_eq!(first.name, "attr1");
                assert_eq!(first.comments_before_equal, &[]);
                assert_eq!(first.comments_after_equal, &[]);
                assert_eq!(first.comments_right, &[]);
                assert_eq!(first_roots, &[1]);

                assert_eq!(second.name, "\"attr2\"");
                assert_eq!(second.comments_before_equal, &[]);
                assert_eq!(second.comments_after_equal, &[]);
                assert_eq!(second.comments_right, &[]);
                assert_eq!(second_roots, &[2]);

                assert_eq!(third.name, "${attr3}");
                assert_eq!(third.comments_before_equal, &[]);
                assert_eq!(third.comments_after_equal, &[]);
                assert_eq!(third.comments_right, &[]);
                assert_eq!(third_roots, &[3]);

                assert_eq!(fourth.name, "\"attr${\"\"}4\"");
                assert_eq!(
                    fourth.comments_before_equal,
                    &[Comment::new("/*before equal*/")]
                );
                assert_eq!(
                    fourth.comments_after_equal,
                    &[Comment::new("/*after equal*/")]
                );
                assert_eq!(
                    fourth.comments_right,
                    &[Comment::new("/*before*/"), Comment::new("/*Right*/")]
                );
                assert_eq!(value.to_string(), "(true)");
            }
            _ => panic!("{attributes:#?}"),
        }
    }
}
