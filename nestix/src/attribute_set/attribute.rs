use std::mem;

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

    /// Returns the name of this attribute.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Merges nested non-recursive attribute sets with the same path under the
    /// attribute at `index`.
    /// Returns `true` if the attribute at `index` was not in multiline format
    /// before, but is in multiline format afterwards.
    ///
    /// # Panics
    ///
    /// Panics if the node at `index` is not an attribute.
    pub(super) fn normalize(nodes: &mut Vec<Node>, index: usize) -> bool {
        let (inline, roots) = match &mut AttributeSet::get_attribute_mut(nodes, index).value {
            AttributeValue::AttributeSet { inline, roots, .. } => (inline, roots),
            _ => return false,
        };

        if roots.is_empty() {
            *inline = false;
            return false;
        }

        *inline = true;

        let mut new_roots = mem::take(roots);
        let contains_new_multiline = AttributeSet::normalize_nodes(nodes, &mut new_roots);

        let (format, roots) = match &mut AttributeSet::get_attribute_mut(nodes, index).value {
            AttributeValue::AttributeSet { format, roots, .. } => (format, roots),
            _ => panic!("node at index {index} should be an attribute"),
        };
        if contains_new_multiline {
            *format = Some(AttributeSetFormat::Multiline)
        }
        *roots = new_roots;

        contains_new_multiline
    }

    /// Merges the attribute at `first` with the attribute at `second`. The
    /// attribute at `second` should not be used afterwards.
    /// Returns `true` if the attribute set at `first` and the attribute set at
    /// `second` are both nonempty.
    ///
    /// # Panics
    ///
    /// Panics if `first == second`, the node at `first` or the node at `second`
    /// is not an attribute, or if the value of the attribute at `first` or the
    /// value of the attribute at `second` is not an attribute set.
    pub(super) fn merge(nodes: &mut Vec<Node>, first: usize, second: usize) -> bool {
        let [first_node, second_node] = nodes
            .get_disjoint_mut([first, second])
            .expect("first != second");
        let (first_attribute, second_attribute) =
            match (&mut first_node.value, &mut second_node.value) {
                (Element::Attribute(first), Element::Attribute(second)) => (first, second),
                _ => panic!("nodes at `first` and `second` should be attributes"),
            };

        let (second_format, second_roots, second_first) = match &mut second_attribute.value {
            AttributeValue::AttributeSet { format, roots, .. } => {
                let first = roots.first().copied();
                (format, roots, first)
            }
            _ => panic!("`second_attribute.value` should be an attribute set"),
        };

        let are_both_nonempty = match &mut first_attribute.value {
            AttributeValue::AttributeSet { format, roots, .. } => {
                let are_both_nonempty = !roots.is_empty() && !second_roots.is_empty();

                if roots.is_empty() && !second_roots.is_empty() {
                    *format = second_format.clone();
                }
                roots.append(second_roots);

                are_both_nonempty
            }
            _ => panic!("`first_attribute.value` should be an attribute set"),
        };

        let mut comments_above = mem::take(&mut second_node.comments_above);
        comments_above.extend(mem::take(&mut second_attribute.comments_before_equal));
        comments_above.extend(mem::take(&mut second_attribute.comments_after_equal));
        comments_above.extend(mem::take(&mut second_attribute.comments_right));
        if let Some(i) = second_first {
            comments_above.extend(mem::take(&mut nodes[i].comments_above));
            nodes[i].comments_above = comments_above;
        } else {
            first_node.comments_above.extend(comments_above);
        }

        are_both_nonempty
    }

    /// Sets the format of this attribute's attribute set to
    /// `AttributeSetFormat::Multiline`.
    ///
    /// # Panics
    ///
    /// Panics if this attribute's value is not an attribute set.
    pub(super) fn set_format_multiline(&mut self) {
        match &mut self.value {
            AttributeValue::AttributeSet { format, .. } => {
                *format = Some(AttributeSetFormat::Multiline)
            }
            AttributeValue::Expression(_) => panic!("attribute's value should be an attribute set"),
        }
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

    fn parse_string_to_nodes(input: &str) -> Vec<Node> {
        let mut nodes = Vec::new();
        match Root::parse(input).ok().unwrap().expr().unwrap() {
            Expr::AttrSet(x) => AttributeSet::construct(&mut nodes, &mut 0, x),
            _ => panic!(),
        };
        nodes
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

    #[test]
    fn normalize() {
        let (mut nodes, _) = parse_string_to_attribute(
            "{attr1.attr2 = {attr3.attr4 = true; attr3.attr5 = true; attr6 = {};};}",
            Vec::new(),
            Vec::new(),
        );
        assert!(Attribute::normalize(&mut nodes, 0));

        let attributes: Vec<_> = nodes
            .into_iter()
            .map(|x| match x.value {
                Element::Attribute(x) => x,
                _ => panic!(),
            })
            .collect();

        match &attributes[..] {
            [
                Attribute {
                    name: first_name,
                    value:
                        AttributeValue::AttributeSet {
                            inline: true,
                            format: Some(AttributeSetFormat::Multiline),
                            roots: first_roots,
                            ..
                        },
                    ..
                },
                Attribute {
                    name: second_name,
                    value:
                        AttributeValue::AttributeSet {
                            inline: true,
                            format: Some(AttributeSetFormat::Multiline),
                            roots: second_roots,
                            ..
                        },
                    ..
                },
                Attribute {
                    name: third_name,
                    value:
                        AttributeValue::AttributeSet {
                            inline: true,
                            format: Some(AttributeSetFormat::Multiline),
                            roots: third_roots,
                        },
                    ..
                },
                Attribute {
                    name: fourth_name,
                    value: AttributeValue::Expression(_),
                    ..
                },
                _,
                Attribute {
                    name: fifth_name,
                    value: AttributeValue::Expression(_),
                    ..
                },
                Attribute {
                    name: sixth_name,
                    value:
                        AttributeValue::AttributeSet {
                            inline: false,
                            format: Some(AttributeSetFormat::Inline { .. }),
                            roots: fourth_roots,
                        },
                    ..
                },
            ] => {
                assert_eq!(first_name, "attr1");
                assert_eq!(second_name, "attr2");
                assert_eq!(third_name, "attr3");
                assert_eq!(fourth_name, "attr4");
                assert_eq!(fifth_name, "attr5");
                assert_eq!(sixth_name, "attr6");

                assert_eq!(first_roots, &[1]);
                assert_eq!(second_roots, &[2, 6]);
                assert_eq!(third_roots, &[3, 5]);
                assert_eq!(fourth_roots, &[]);
            }
            _ => panic!("{attributes:#?}"),
        };
    }

    #[test]
    #[should_panic = "node at index 0 should be an attribute"]
    fn normalize_invalid() {
        let mut nodes = parse_string_to_nodes("{/* Comment */}");
        Attribute::normalize(&mut nodes, 0);
    }

    #[test]
    fn merge_empty() {
        let mut nodes = parse_string_to_nodes(
            "{/*above first*/attr1 = {attr2 = true;};\n/*above second*/attr1/*before*/=/*after*/{ }/*right*/;}",
        );
        assert!(!Attribute::merge(&mut nodes, 0, 2));
        match &nodes[..] {
            [
                Node {
                    group: 0,
                    comments_above: comments_above_first,
                    value:
                        Element::Attribute(Attribute {
                            name: first_name,
                            value:
                                AttributeValue::AttributeSet {
                                    inline: false,
                                    format:
                                        Some(AttributeSetFormat::Inline {
                                            surrounding_whitespace,
                                        }),
                                    roots,
                                },
                            ..
                        }),
                },
                Node {
                    group: 0,
                    comments_above: comments_above_second,
                    value:
                        Element::Attribute(Attribute {
                            name: second_name,
                            value: AttributeValue::Expression(_),
                            ..
                        }),
                },
                _,
            ] => {
                assert_eq!(first_name, "attr1");
                assert_eq!(second_name, "attr2");
                assert_eq!(
                    comments_above_first,
                    &[
                        Comment::new("/*above first*/"),
                        Comment::new("/*above second*/"),
                        Comment::new("/*before*/"),
                        Comment::new("/*after*/"),
                        Comment::new("/*right*/"),
                    ]
                );
                assert_eq!(comments_above_second, &[]);
                assert_eq!(surrounding_whitespace, "");
                assert_eq!(roots, &[1]);
            }
            _ => panic!("{nodes:#?}"),
        };

        let mut nodes = parse_string_to_nodes(
            "{/*above first*/attr1 = {};\n/*above second*/attr1/*before*/=/*after*/{ /*above attr2*/attr2 = true; }/*right*/;}",
        );
        assert!(!Attribute::merge(&mut nodes, 0, 1));
        match &nodes[..] {
            [
                Node {
                    group: 0,
                    comments_above: comments_above_first,
                    value:
                        Element::Attribute(Attribute {
                            name: first_name,
                            value:
                                AttributeValue::AttributeSet {
                                    inline: false,
                                    format:
                                        Some(AttributeSetFormat::Inline {
                                            surrounding_whitespace,
                                        }),
                                    roots,
                                },
                            ..
                        }),
                },
                _,
                Node {
                    group: 0,
                    comments_above: comments_above_second,
                    value:
                        Element::Attribute(Attribute {
                            name: second_name,
                            value: AttributeValue::Expression(_),
                            ..
                        }),
                },
            ] => {
                assert_eq!(first_name, "attr1");
                assert_eq!(second_name, "attr2");
                assert_eq!(comments_above_first, &[Comment::new("/*above first*/")]);
                assert_eq!(
                    comments_above_second,
                    &[
                        Comment::new("/*above second*/"),
                        Comment::new("/*before*/"),
                        Comment::new("/*after*/"),
                        Comment::new("/*right*/"),
                        Comment::new("/*above attr2*/"),
                    ]
                );
                assert_eq!(surrounding_whitespace, " ");
                assert_eq!(roots, &[2]);
            }
            _ => panic!("{nodes:#?}"),
        }
    }

    #[test]
    fn merge_filled() {
        let mut nodes = parse_string_to_nodes(
            "{attr1.attr2 = true;\n/*above attr1*/attr1/*before*/=/*after*/{/*above attr3*/attr3/*before equal*/= true; attr4 = true;}/*right*/;}",
        );
        assert!(Attribute::merge(&mut nodes, 0, 2));

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
                                    inline: true,
                                    format: None,
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
                            value: AttributeValue::Expression(_),
                            ..
                        }),
                },
                _,
                Node {
                    group: 0,
                    comments_above,
                    value:
                        Element::Attribute(Attribute {
                            name: third_name,
                            comments_before_equal,
                            comments_after_equal,
                            comments_right,
                            value: AttributeValue::Expression(_),
                        }),
                    ..
                },
                Node {
                    group: 0,
                    comments_above: _,
                    value:
                        Element::Attribute(Attribute {
                            name: fourth_name,
                            value: AttributeValue::Expression(_),
                            ..
                        }),
                },
            ] => {
                assert_eq!(first_name, "attr1");
                assert_eq!(second_name, "attr2");
                assert_eq!(third_name, "attr3");
                assert_eq!(fourth_name, "attr4");

                assert_eq!(roots, &[1, 3, 4]);

                assert_eq!(
                    comments_above,
                    &[
                        Comment::new("/*above attr1*/"),
                        Comment::new("/*before*/"),
                        Comment::new("/*after*/"),
                        Comment::new("/*right*/"),
                        Comment::new("/*above attr3*/")
                    ]
                );
                assert_eq!(comments_before_equal, &[Comment::new("/*before equal*/")]);
                assert_eq!(comments_after_equal, &[]);
                assert_eq!(comments_right, &[]);
            }
            _ => panic!("{nodes:#?}"),
        }
    }

    #[test]
    #[should_panic = "first != second"]
    fn merge_invalid_indices() {
        let mut nodes = parse_string_to_nodes("{attr1 = {};}");
        Attribute::merge(&mut nodes, 0, 0);
    }

    #[test]
    #[should_panic = "nodes at `first` and `second` should be attributes"]
    fn merge_invalid_non_attributes() {
        let mut nodes = parse_string_to_nodes("{# first\n\nsecond = true;}");
        Attribute::merge(&mut nodes, 0, 1);
    }

    #[test]
    #[should_panic = "`first_attribute.value` should be an attribute set"]
    fn merge_invalid_first_value() {
        let mut nodes = parse_string_to_nodes("{attr1 = true; attr2 = {};}");
        Attribute::merge(&mut nodes, 0, 1);
    }

    #[test]
    #[should_panic = "`second_attribute.value` should be an attribute set"]
    fn merge_invalid_second_value() {
        let mut nodes = parse_string_to_nodes("{attr1 = {}; attr2 = true;}");
        Attribute::merge(&mut nodes, 0, 1);
    }
}
