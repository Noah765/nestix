use std::{collections::HashMap, ops::RangeInclusive};

use rnix::{
    SyntaxElement, SyntaxKind,
    ast::{self, AttrSet, AttrpathValue},
};
use rowan::ast::AstNode;

use crate::{
    attribute_set::{attribute::Attribute, format::AttributeSetFormat, inherit::Inherit},
    comment::Comment,
    formatter::Formatter,
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
    indentation_level: usize,
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
    /// Constructs a new `AttributeSet` based on `node` and `indentation_level`.
    ///
    /// `indentation_level` is the syntax-tree indentation level of `node`. The
    /// indentation level of an attribute set that is not indented is `0`.
    pub fn new(node: AttrSet, indentation_level: usize) -> Self {
        let mut parser = Parser::new(node.syntax().clone());
        let (recursive, comments_after_rec) = match parser.next() {
            None => panic!("`node` should have children"),
            Some(x) if x.kind() == SyntaxKind::TOKEN_REC => (true, parser.next_comments()),
            Some(_) => (false, Vec::new()),
        };

        let mut nodes = Vec::new();
        let mut group = 0;
        let (format, roots) = Self::construct(&mut nodes, indentation_level, &mut group, node);

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
        indentation_level: usize,
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
                    indentation_level: indentation_level + 1,
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
                    indentation_level: indentation_level + 1,
                    group: *group,
                    comments_above: comments,
                    value: Element::Inherit(inherit),
                });
            } else {
                let node = AttrpathValue::cast(node).expect("`node` should be an attribute node");
                if Attribute::construct(
                    nodes,
                    indentation_level + 1,
                    group,
                    node,
                    comments,
                    parser.next_comment_line(),
                ) {
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

    /// Returns an immutable reference to the attribute at index `index`.
    ///
    /// # Panics
    ///
    /// Panics if `index` is invalid or the node at that index is not an
    /// attribute.
    fn get_attribute(nodes: &[Node], index: usize) -> &Attribute {
        match &nodes[index].value {
            Element::Attribute(x) => x,
            _ => panic!("node at index {index} should be an attribute"),
        }
    }

    /// Returns a mutable reference to the attribute at index `index`.
    ///
    /// # Panics
    ///
    /// Panics if `index` is invalid or the node at that index is not an
    /// attribute.
    fn get_attribute_mut(nodes: &mut [Node], index: usize) -> &mut Attribute {
        match &mut nodes[index].value {
            Element::Attribute(x) => x,
            _ => panic!("node at index {index} should be an attribute"),
        }
    }

    /// Merges nested non-recursive attribute sets with the same path.
    pub fn normalize(&mut self) {
        if Self::normalize_nodes(&mut self.nodes, &mut self.roots) {
            self.format = AttributeSetFormat::Multiline;
        }
    }

    /// Merges nested non-recursive attribute sets with the same path under
    /// `nodes`.
    /// Returns `true` if `roots` contains any attribute sets in multiline
    /// format that were not previously in that format.
    fn normalize_nodes(nodes: &mut Vec<Node>, roots: &mut Vec<usize>) -> bool {
        let mut contains_new_multiline = false;
        let mut branches = HashMap::new();
        let mut i = 0;

        while i < roots.len() {
            let Element::Attribute(x) = &nodes[roots[i]].value else {
                i += 1;
                continue;
            };

            let Some(&first) = branches.get(x.name()) else {
                branches.insert(String::from(x.name()), roots[i]);
                i += 1;
                continue;
            };

            if Attribute::merge(nodes, first, roots[i]) {
                contains_new_multiline = true;
                match &mut nodes[first].value {
                    Element::Attribute(x) => x.set_format_multiline(),
                    _ => panic!("node at index {i} should be an attribute"),
                }
            }
            roots.remove(i);
        }

        for (_, i) in branches {
            if Attribute::normalize(nodes, i) {
                contains_new_multiline = true;
            }
        }

        contains_new_multiline
    }

    /// Formats this attribute set.
    pub fn format(&mut self) {
        self.normalize();
        Self::format_roots(&mut self.nodes, &self.roots);
    }

    /// Formats the given attribute set roots.
    fn format_roots(nodes: &mut Vec<Node>, roots: &Vec<usize>) {
        for &i in roots {
            if let Element::Attribute(x) = &nodes[i].value
                && x.is_branch()
            {
                Self::format_layer(nodes, vec![i], 2);
            }
        }
    }

    /// Expands inline branch attribute sets in `layer` until the next attribute
    /// tree layer contains at most `max_leaves` attributes. Additionally,
    /// expands attribute sets in `layer` which contain an inherit node.
    ///
    /// # Panics
    ///
    /// Panics if any index in `layer` points to a non-attribute node or
    /// an attribute whose value is not an attribute set. Also panics if
    /// `layer.len() > max_leaves`.
    fn format_layer(nodes: &mut Vec<Node>, layer: Vec<usize>, mut max_leaves: usize) {
        if layer.is_empty() {
            return;
        }

        for &i in &layer {
            if Self::get_attribute(nodes, i).contains_inherit(nodes) {
                *Self::get_attribute_mut(nodes, i).inline_mut() = false;
                Attribute::format(nodes, i);
                max_leaves -= 1;
            }
        }

        let layer: Vec<_> = layer
            .iter()
            .filter_map(|&i| {
                let x = Self::get_attribute(nodes, i);
                x.inline().then(|| (i, x.count_nested_attributes(nodes)))
            })
            .collect();

        let mut next_layer_size: usize = layer.iter().map(|x| x.1).sum();

        while next_layer_size > max_leaves {
            let &(i, size) = layer
                .iter()
                .filter(|x| Self::get_attribute(nodes, x.0).inline())
                .max_by_key(|x| x.1)
                .expect("`next_layer_size <= max_leaves` should be reachable");
            let x = Self::get_attribute_mut(nodes, i);

            *x.inline_mut() = false;
            Attribute::format(nodes, i);

            max_leaves -= 1;
            next_layer_size -= size;
        }

        let (next_layer, leaves_count) = layer
            .into_iter()
            .filter(|x| Self::get_attribute(nodes, x.0).inline())
            .map(|x| Self::get_attribute(nodes, x.0).get_nested_branches_and_leave_count(nodes))
            .fold((Vec::new(), 0), |mut acc, x| {
                acc.0.extend(x.0);
                acc.1 += x.1;
                acc
            });
        max_leaves -= leaves_count;
        Self::format_layer(nodes, next_layer, max_leaves);
    }

    /// Writes this attribute set to the formatter using its current format.
    pub fn print(&self, formatter: &mut Formatter) {
        if self.recursive {
            formatter.write("rec ");
            for x in &self.comments_after_rec {
                x.print(formatter);
                formatter.write(" ");
            }
        }

        Self::print_roots(formatter, &self.nodes, &self.roots, &self.format);
    }

    /// Writes the `roots` as an attribute set to `formatter` using `format` and
    /// returns the group of the last nested attribute.
    fn print_roots(
        formatter: &mut Formatter,
        nodes: &Vec<Node>,
        roots: &[usize],
        format: &AttributeSetFormat,
    ) -> Option<usize> {
        formatter.write("{");
        match format {
            AttributeSetFormat::Inline {
                surrounding_whitespace,
            } => formatter.write(surrounding_whitespace),
            AttributeSetFormat::Multiline => formatter.increase_indentation(),
        }

        if roots.is_empty() {
            if *format == AttributeSetFormat::Multiline {
                formatter.decrease_indentation();
                formatter.open_line()
            }
            formatter.write("}");
            return None;
        }

        let mut elements: Vec<_> = roots
            .iter()
            .flat_map(|&i| Self::print_node_at(formatter, nodes, i))
            .collect();
        elements.sort_unstable_by_key(|x| x.0);

        let last_group = elements.last().map(|x| *x.1.end());

        match format {
            AttributeSetFormat::Inline {
                surrounding_whitespace,
            } => {
                let last_index = elements.len() - 1;
                for (i, x) in elements.into_iter().enumerate() {
                    for x in x.2 {
                        x.print(formatter);
                        formatter.write(" ");
                    }
                    formatter.write(&x.3);
                    if i != last_index {
                        formatter.write(" ");
                    }
                }

                formatter.write(surrounding_whitespace);
            }
            AttributeSetFormat::Multiline => {
                let mut group = *elements[0].1.start();
                for x in elements {
                    if group != *x.1.start() {
                        formatter.write("\n");
                    }
                    group = *x.1.end();

                    for x in x.2 {
                        formatter.open_line();
                        x.print(formatter);
                    }

                    formatter.open_line();
                    formatter.write(&x.3);
                }

                formatter.decrease_indentation();
                formatter.open_line();
            }
        }

        formatter.write("}");

        last_group
    }

    /// Returns the printed elements of the node at `index`, formatting
    /// attribute values but not the surrounding attribute set.
    fn print_node_at<'a>(
        formatter: &Formatter,
        nodes: &'a Vec<Node>,
        index: usize,
    ) -> Vec<(usize, RangeInclusive<usize>, Vec<&'a Comment>, String)> {
        match &nodes[index].value {
            Element::Inherit(x) => {
                let mut formatter = formatter.child_formatter();
                x.print(&mut formatter);
                vec![(
                    index,
                    nodes[index].group..=nodes[index].group,
                    nodes[index].comments_above.iter().collect(),
                    formatter.into_string(),
                )]
            }
            Element::Attribute(x) => x.print(formatter, nodes, index),
            Element::Comment(x) => {
                let mut formatter = formatter.child_formatter();
                x.print(&mut formatter);
                vec![(
                    index,
                    nodes[index].group..=nodes[index].group,
                    nodes[index].comments_above.iter().collect(),
                    formatter.into_string(),
                )]
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rnix::{Root, ast::Expr};

    use crate::formatter::IndentationType;

    use super::*;

    fn parse_string_to_set(input: &str) -> AttributeSet {
        match Root::parse(input).ok().unwrap().expr().unwrap() {
            Expr::AttrSet(x) => AttributeSet::new(x, 0),
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
                    indentation_level: 1,
                    group: 0,
                    comments_above: comments_above_first,
                    value: Element::Comment(_),
                },
                Node {
                    indentation_level: 1,
                    group: 0,
                    comments_above: comments_above_second,
                    value: Element::Comment(_),
                },
                Node {
                    indentation_level: 1,
                    group: 1,
                    comments_above: comments_above_third,
                    value: Element::Attribute(_),
                },
                Node {
                    indentation_level: 1,
                    group: 1,
                    comments_above: comments_above_fourth,
                    value: Element::Attribute(_),
                },
                Node {
                    indentation_level: 1,
                    group: 2,
                    comments_above: comments_above_fifth,
                    value: Element::Inherit(_),
                },
                Node {
                    indentation_level: 1,
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

    #[test]
    fn normalize() {
        let mut set = parse_string_to_set(
            "{attr1 = {attr2 = true;}; inherit; attr1 = {attr3.attr4 = true; attr3.attr5 = true;};}",
        );
        set.normalize();
        assert_eq!(set.format, AttributeSetFormat::Multiline);

        let mut set = parse_string_to_set("{attr1 = {attr4.attr5 = true; attr4.attr6 = true;};}");
        set.normalize();
        assert_eq!(set.format, AttributeSetFormat::Multiline);
    }

    #[test]
    fn format() {
        fn test(input: &str, expected: &[bool]) {
            let mut set = parse_string_to_set(input);
            set.format();
            let got: Vec<_> = set
                .nodes
                .into_iter()
                .filter_map(|x| match x.value {
                    Element::Attribute(x) if x.is_branch() => Some(x.inline()),
                    _ => None,
                })
                .collect();
            assert_eq!(got, expected, "{input}");
        }
        test(
            "{attr1 = {attr2 = {attr3 = true; attr4 = true;}; # Comment\n\nattr5 = {};};}",
            &[true, false],
        );
        test(
            "{attr1 = {attr2 = {attr3 = true; attr4 = true;}; inherit; attr5 = {};};}",
            &[false, true],
        );
        test(
            "{attr1 = {attr2 = {attr3 = true; attr4 = true;}; attr5 = {inherit;};};}",
            &[true, false, false],
        );
        test(
            "{attr1 = {attr2.attr3 = true; attr4 = {attr5 = true; attr6 = true;};};}",
            &[true, true, false],
        );
        test(
            "{attr1 = {inherit; attr2 = {attr3 = true; attr4 = true; attr5 = true;};};}",
            &[false, false],
        );
    }

    #[test]
    #[should_panic = "should be an attribute"]
    fn format_layer_invalid_non_attributes() {
        let mut set = parse_string_to_set("{/*Comment*/}");
        AttributeSet::format_layer(&mut set.nodes, vec![0], 2);
    }

    #[test]
    #[should_panic = "should be an attribute set"]
    fn format_layer_invalid_non_attribute_sets() {
        let mut set = parse_string_to_set("{attr1 = true;}");
        AttributeSet::format_layer(&mut set.nodes, vec![0], 2);
    }

    #[test]
    fn print() {
        fn test(input: &str, expected: &str) {
            let mut formatter = Formatter::new(IndentationType::TwoSpaces);
            parse_string_to_set(input).print(&mut formatter);
            assert_eq!(formatter.into_string(), expected, "{input}");
        }
        test("rec\n/*after*/\n{ }", "rec /*after*/ { }");
        test(
            "{attr1 = {\n};\n/*comment*/\n\nattr2 = true;}",
            "{\n  attr1 = {\n  };\n  /*comment*/\n\n  attr2 = true;\n}",
        );
        test(
            "{attr1 = true;\n\n\nattr2 = true;}",
            "{\n  attr1 = true;\n\n  attr2 = true;\n}",
        );
    }
}
