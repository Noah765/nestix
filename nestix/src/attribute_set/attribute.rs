use rnix::ast::{AttrpathValue, Expr};
use rowan::ast::AstNode;

use crate::{
    attribute_set::{AttributeSet, Element, Node, format::AttributeSetFormat},
    comment::Comment,
    parser::Parser,
};

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
    /// Returns `true` if the value of `node` is a non-recursive attribute set
    /// in multiline format.
    pub fn construct(
        nodes: &mut Vec<Node>,
        group: &mut usize,
        node: AttrpathValue,
        mut comments_above: Vec<Comment>,
        comments_right: Vec<Comment>,
    ) -> bool {
        let first_index = nodes.len();

        let mut parser = Parser::new(node.syntax().clone());
        let mut path_parser = Parser::new(parser.next_attribute_path().syntax().clone());

        while path_parser.peek().is_some() {
            let mut comments_above = path_parser.next_comments();
            let name = path_parser.next_attribute().to_string();
            comments_above.extend(path_parser.next_comments());

            nodes.push(Node {
                group: *group,
                comments_above,
                value: Element::Attribute(Self {
                    name,
                    comments_before_equal: Vec::new(),
                    comments_after_equal: Vec::new(),
                    comments_right: Vec::new(),
                    value: AttributeValue::AttributeSet {
                        inline: true,
                        format: None,
                        roots: vec![nodes.len() + 1],
                    },
                }),
            });

            path_parser.next();
        }

        comments_above.append(&mut nodes[first_index].comments_above);
        nodes[first_index].comments_above = comments_above;

        let last_index = nodes.len() - 1;
        let last = AttributeSet::get_attribute_mut(nodes, last_index);

        last.comments_before_equal = parser.next_comments();
        parser.next();
        last.comments_after_equal = parser.next_comments();

        let value = parser.next_expression();
        let mut unwrapped_value = value.clone();
        while let Expr::Paren(x) = unwrapped_value {
            unwrapped_value = x.expr().unwrap();
        }
        let (value, contains_multiline) = match unwrapped_value {
            Expr::AttrSet(x) if x.rec_token().is_none() => {
                let (format, roots) = AttributeSet::construct(nodes, group, x);
                let contains_multiline = format == AttributeSetFormat::Multiline;
                let value = AttributeValue::AttributeSet {
                    inline: false,
                    format: Some(format),
                    roots,
                };
                (value, contains_multiline)
            }
            _ => (AttributeValue::Expression(value), false),
        };
        let last = AttributeSet::get_attribute_mut(nodes, last_index);
        last.value = value;

        last.comments_right = parser.next_comments();
        last.comments_right.extend(comments_right);

        contains_multiline
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rnix::{Root, ast::HasEntry};

    use super::*;

    fn parse_string_to_attribute(
        input: &str,
        comments_above: Vec<&str>,
        comments_right: Vec<&str>,
    ) -> (Vec<Node>, bool) {
        let mut nodes = Vec::new();
        let contains_multiline = match Root::parse(input).ok().unwrap().expr().unwrap() {
            Expr::AttrSet(x) => Attribute::construct(
                &mut nodes,
                &mut 0,
                x.attrpath_values().next().unwrap(),
                comments_above.into_iter().map(Comment::new).collect(),
                comments_right.into_iter().map(Comment::new).collect(),
            ),
            _ => panic!(),
        };
        (nodes, contains_multiline)
    }

    #[test]
    fn construct_expression_value() {
        let (nodes, _) = parse_string_to_attribute(
            "{/*Above*/attr1/*before*/./*after*/\"attr2\"/*before next*/.${attr3}./*before*/\"attr${\"\"}4\"/*before equal*/=/*after equal*/(true)/*before*/;/*Right*/}",
            vec!["/*Above*/"],
            vec!["/*Right*/"],
        );

        assert_eq!(
            nodes[0].comments_above,
            [Comment::new("/*Above*/"), Comment::new("/*before*/")]
        );
        assert_eq!(
            nodes[1].comments_above,
            [Comment::new("/*after*/"), Comment::new("/*before next*/")]
        );
        assert_eq!(nodes[2].comments_above, Vec::new());
        assert_eq!(nodes[3].comments_above, vec![Comment::new("/*before*/")]);

        let attributes: Vec<_> = nodes
            .into_iter()
            .map(|x| match x {
                Node {
                    value: Element::Attribute(x),
                    ..
                } => x,
                _ => panic!(),
            })
            .collect();

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

    #[test]
    fn construct_empty_attribute_set_value() {
        let (nodes, _) = parse_string_to_attribute("{attr1 = {};}", Vec::new(), Vec::new());

        match &nodes[..] {
            [
                Node {
                    group: 0,
                    comments_above: _,
                    value:
                        Element::Attribute(Attribute {
                            name,
                            value:
                                AttributeValue::AttributeSet {
                                    inline: false,
                                    format: Some(AttributeSetFormat::Inline { .. }),
                                    roots,
                                },
                            ..
                        }),
                },
            ] => {
                assert_eq!(name.to_string(), "attr1");
                assert_eq!(roots, &[]);
            }
            _ => panic!("{nodes:#?}"),
        }
    }

    #[test]
    fn construct_filled_attribute_set_value() {
        let (nodes, _) = parse_string_to_attribute(
            "{attr1 = (({attr2 = true; attr3 = true;}));}",
            Vec::new(),
            Vec::new(),
        );

        match &nodes[..] {
            [
                Node {
                    group: 0,
                    comments_above: _,
                    value:
                        Element::Attribute(Attribute {
                            name: first_name,
                            value:
                                AttributeValue::AttributeSet {
                                    inline: false,
                                    format: Some(AttributeSetFormat::Inline { .. }),
                                    roots,
                                },
                            ..
                        }),
                },
                Node {
                    group: 0,
                    comments_above: _,
                    value:
                        Element::Attribute(Attribute {
                            name: second_name,
                            value: AttributeValue::Expression(first_value),
                            ..
                        }),
                },
                Node {
                    group: 0,
                    comments_above: _,
                    value:
                        Element::Attribute(Attribute {
                            name: third_name,
                            value: AttributeValue::Expression(second_value),
                            ..
                        }),
                },
            ] => {
                assert_eq!(first_name, "attr1");
                assert_eq!(second_name, "attr2");
                assert_eq!(third_name, "attr3");
                assert_eq!(roots, &[1, 2]);
                assert_eq!(first_value.to_string(), "true");
                assert_eq!(second_value.to_string(), "true");
            }
            _ => panic!("{nodes:#?}"),
        }
    }

    #[test]
    fn construct_recursive_attribute_set_value() {
        let (nodes, _) =
            parse_string_to_attribute("{attr1 = rec {attr2 = true;};}", Vec::new(), Vec::new());

        match &nodes[..] {
            [
                Node {
                    group: 0,
                    comments_above: _,
                    value:
                        Element::Attribute(Attribute {
                            value: AttributeValue::Expression(value),
                            ..
                        }),
                },
            ] => {
                assert_eq!(value.to_string(), "rec {attr2 = true;}");
            }
            _ => panic!("{nodes:#?}"),
        }
    }

    #[test]
    fn construct_return_value() {
        fn test(input: &str, expected: bool) {
            let (_, contains_multiline) = parse_string_to_attribute(input, Vec::new(), Vec::new());
            assert_eq!(contains_multiline, expected, "{input}");
        }
        test("{attr1 = {\n  attr2 = true;\n};}", true);
        test("{attr1 = {attr2 = true;};}", false);
        test("{attr1 = rec {\n  attr2 = true;\n};}", false);
        test("{attr1 = true;}", false);
    }
}
