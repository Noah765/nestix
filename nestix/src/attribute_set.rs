use rnix::{
    SyntaxElement, SyntaxKind,
    ast::{self, AttrSet, AttrpathValue},
};
use rowan::ast::AstNode;

use crate::{
    attribute_set::{attribute::Attribute, format::AttributeSetFormat, inherit::Inherit},
    comment::Comment,
    parser::Parser,
};

mod attribute;
pub mod format;
pub mod inherit;

/// A Nix attribute set.
//
// INVARIANT: `format` is `AttributeSetFormat::Multiline` whenever there are
// nested non-recursive multiline attribute sets.
#[derive(Clone, Debug)]
pub struct AttributeSet {
    format: AttributeSetFormat,
    recursive: bool,
    comments_after_rec: Vec<Comment>,
    roots: Vec<usize>,
    nodes: Vec<Node>,
}

#[derive(Clone, Debug)]
struct Node {
    group: usize,
    comments_above: Vec<Comment>,
    value: Element,
}

#[derive(Clone, Debug)]
enum Element {
    Inherit(Inherit),
    Attribute(Attribute),
    Comment(Comment),
}

impl AttributeSet {
    /// Constructs a new `AttributeSet` based on `node`.
    pub fn new(node: AttrSet) -> Self {
        let mut parser = Parser::new(node.syntax().clone());
        let (recursive, comments_after_rec) = match parser.next() {
            None => panic!("`node` should have children"),
            Some(x) if x.kind() == SyntaxKind::TOKEN_REC => (true, parser.next_comments()),
            Some(_) => (false, Vec::new()),
        };

        let mut nodes = Vec::new();
        let mut group = 0;
        let (format, roots) = Self::construct(&mut nodes, &mut group, node);

        Self {
            format,
            recursive,
            comments_after_rec,
            roots,
            nodes,
        }
    }

    /// Extends `nodes` by elements constructed from `node` and returns the
    /// format of `node` as well as its root nodes.
    fn construct(
        nodes: &mut Vec<Node>,
        group: &mut usize,
        node: AttrSet,
    ) -> (AttributeSetFormat, Vec<usize>) {
        let mut is_multiline = false;
        let mut roots = Vec::new();

        let mut parser = Parser::new(node.syntax().clone());
        parser.skip_after(|x| x.kind() == SyntaxKind::TOKEN_L_BRACE);
        parser.skip_whitespace();

        while let Some(x) = parser.peek()
            && x.kind() != SyntaxKind::TOKEN_R_BRACE
        {
            let comments = parser.next_comment_section();
            let element = parser
                .next()
                .expect("`parser` should be somewhere before the `}` token");

            let SyntaxElement::Node(node) = element else {
                roots.extend(nodes.len()..nodes.len() + comments.len());
                nodes.extend(comments.into_iter().map(|x| Node {
                    group: *group,
                    comments_above: Vec::new(),
                    value: Element::Comment(x),
                }));
                *group += 1;
                continue;
            };

            roots.push(nodes.len());

            if ast::Inherit::can_cast(node.kind()) {
                let node = ast::Inherit::cast(node).unwrap();
                let inherit = Inherit::new(node, parser.next_comment_line());
                nodes.push(Node {
                    group: *group,
                    comments_above: comments,
                    value: Element::Inherit(inherit),
                });
            } else {
                let node = AttrpathValue::cast(node).expect("`node` should be an attribute node");
                if Attribute::construct(nodes, group, node, comments, parser.next_comment_line()) {
                    is_multiline = true;
                }
            }

            if parser.next_whitespace().matches('\n').nth(1).is_some() {
                *group += 1;
            }
        }

        let format = if is_multiline {
            AttributeSetFormat::Multiline
        } else {
            AttributeSetFormat::new(node)
        };
        (format, roots)
    }

    /// Returns a mutable reference to the attribute at index `index`.
    ///
    /// # Panics
    ///
    /// Panics if `index` is invalid or the node at that index is not an
    /// attribute.
    fn get_attribute_mut(nodes: &mut Vec<Node>, index: usize) -> &mut Attribute {
        match &mut nodes[index].value {
            Element::Attribute(x) => x,
            _ => panic!("node at index {index} should be an attribute"),
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rnix::{Root, ast::Expr};

    use super::*;

    fn parse_string_to_set(input: &str) -> AttributeSet {
        match Root::parse(input).ok().unwrap().expr().unwrap() {
            Expr::AttrSet(x) => AttributeSet::new(x),
            _ => panic!(),
        }
    }

    #[test]
    fn new_empty() {
        let set = parse_string_to_set("{\n\n}");
        assert_eq!(set.format, AttributeSetFormat::Multiline);
        assert!(!set.recursive);
        assert_eq!(set.comments_after_rec, []);
        assert_eq!(set.roots, []);
        assert!(set.nodes.is_empty());
    }

    #[test]
    fn new_filled() {
        let set = parse_string_to_set(
            "rec/*after rec*/{#Comment\n#Second\n\t\n#Above attr1\n/*Above attr1*/attr1./*above*/attr2 = true; #Right of attr1\n\n/*Above inherit*/inherit a;/*Right of inherit*/\n/*Last*/}",
        );
        assert_eq!(set.format, AttributeSetFormat::Multiline);
        assert!(set.recursive);
        assert_eq!(set.comments_after_rec, [Comment::new("/*after rec*/")]);
        assert_eq!(set.roots, [0, 1, 2, 4, 5]);

        match &set.nodes[..] {
            [
                Node {
                    group: 0,
                    comments_above: comments_above_first,
                    value: Element::Comment(_),
                },
                Node {
                    group: 0,
                    comments_above: comments_above_second,
                    value: Element::Comment(_),
                },
                Node {
                    group: 1,
                    comments_above: comments_above_third,
                    value: Element::Attribute(_),
                },
                Node {
                    group: 1,
                    comments_above: comments_above_fourth,
                    value: Element::Attribute(_),
                },
                Node {
                    group: 2,
                    comments_above: comments_above_fifth,
                    value: Element::Inherit(_),
                },
                Node {
                    group: 2,
                    comments_above: comments_above_sixth,
                    value: Element::Comment(_),
                },
            ] => {
                assert_eq!(comments_above_first, &[]);
                assert_eq!(comments_above_second, &[]);
                assert_eq!(
                    comments_above_third,
                    &[
                        Comment::new("#Above attr1"),
                        Comment::new("/*Above attr1*/"),
                    ]
                );
                assert_eq!(comments_above_fourth, &[Comment::new("/*above*/"),]);
                assert_eq!(comments_above_fifth, &[Comment::new("/*Above inherit*/")]);
                assert_eq!(comments_above_sixth, &[]);
            }
            _ => panic!("{set:#?}"),
        }
    }

    #[test]
    fn new_format() {
        let set = parse_string_to_set("{ attr1 = true; attr2 = {attr3 = true; attr4 = true;}; }");
        assert_eq!(
            set.format,
            AttributeSetFormat::Inline {
                surrounding_whitespace: String::from(" ")
            }
        );

        let set = parse_string_to_set(
            "{attr1 = true; attr2 = {attr3 = true;\nattr4 = true;}; attr5 = {attr6 = true;};}",
        );
        assert_eq!(set.format, AttributeSetFormat::Multiline);
    }
}
