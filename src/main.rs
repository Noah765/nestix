use std::fs;

use rnix::{
    NodeOrToken, Root, SyntaxKind, SyntaxNode,
    ast::{AstToken, Attr, AttrSet, Attrpath, AttrpathValue, Expr, InterpolPart},
};
use rowan::ast::AstNode;

const MAX_ATTRS_LEAVES: usize = 2;

fn main() {
    let file = fs::read_to_string("flake.nix").unwrap();
    let formatted_file = format_node(&file, Root::parse(&file).syntax());
    println!("{formatted_file}");
}

fn format_node(file: &str, node: SyntaxNode) -> String {
    match AttrSet::cast(node.clone()) {
        None => node
            .children_with_tokens()
            .fold(String::new(), |acc, x| match x {
                NodeOrToken::Node(x) => acc + &format_node(file, x),
                NodeOrToken::Token(x) => acc + x.text(),
            }),
        Some(x) => {
            let mut tree = AttrTree::new(x.clone());
            tree.normalize();
            tree.format();
            tree.print(file)
        }
    }
}

#[derive(Debug)]
struct AttrTree {
    attr_set: AttrSet,
    root: AttrTreeBranch,
    leaves: Vec<AttrTreeLeaf>,
}

#[derive(Debug)]
struct AttrTreeBranch {
    key: Option<String>,
    key_text: String,
    inline: bool,
    before_empty_line: bool,
    leaves: Vec<usize>,
    children: Vec<AttrTreeBranch>,
}

#[derive(Debug)]
struct AttrTreeLeaf {
    key_text: String,
    value: SyntaxNode,
    before_empty_line: bool,
}

impl AttrTree {
    fn new(attr_set: AttrSet) -> Self {
        let mut leaves = Vec::new();
        let root = AttrTreeBranch::from_attr_set(attr_set.clone(), &mut leaves);
        Self {
            attr_set,
            root,
            leaves,
        }
    }

    fn normalize(&mut self) {
        self.root.normalize();
    }

    fn format(&mut self) {
        self.root.format(&mut self.leaves);
    }

    fn print(&self, file: &str) -> String {
        let format = AttrSetFormat::new(file, self.attr_set.clone());
        self.root.print_attr_set(&self.leaves, file, format).0
    }
}

impl AttrTreeBranch {
    fn new(key: Option<String>, key_text: String, before_empty_line: bool) -> Self {
        Self {
            key,
            key_text,
            inline: true,
            before_empty_line,
            leaves: Vec::new(),
            children: Vec::new(),
        }
    }

    fn from_attr_set(attr_set: AttrSet, leaves: &mut Vec<AttrTreeLeaf>) -> AttrTreeBranch {
        let mut root = AttrTreeBranch::new(None, String::new(), false);

        let mut children = attr_set
            .syntax()
            .children_with_tokens()
            .skip_while(|x| matches!(x, NodeOrToken::Token(_)))
            .peekable();

        while let Some(NodeOrToken::Node(attr_path_value)) = children.next() {
            let attr_path_value = AttrpathValue::cast(attr_path_value).unwrap();

            let before_empty_line = match children.peek() {
                None | Some(NodeOrToken::Node(_)) => false,
                Some(NodeOrToken::Token(x)) => x.text().matches('\n').count() >= 2,
            };
            if let Some(NodeOrToken::Token(_)) = children.peek() {
                children.next();
            }

            root.add_attr_path_value(leaves, attr_path_value, before_empty_line);
        }

        root
    }

    fn add_attr_path_value(
        &mut self,
        leaves: &mut Vec<AttrTreeLeaf>,
        attr_path_value: AttrpathValue,
        before_empty_line: bool,
    ) {
        let node = Self::from_attr_path(attr_path_value.attrpath().unwrap(), before_empty_line);
        self.children.push(node);

        let mut leaf_parent = self;
        while !leaf_parent.children.last().unwrap().children.is_empty() {
            leaf_parent = leaf_parent.children.last_mut().unwrap();
        }

        let mut value = attr_path_value.value().unwrap();
        while let Expr::Paren(x) = value {
            value = x.expr().unwrap();
        }

        if let Expr::AttrSet(attr_set) = value.clone()
            && attr_set.rec_token().is_none()
        {
            let subtree = Self::from_attr_set(attr_set, leaves);
            let leaf = leaf_parent.children.last_mut().unwrap();
            leaf.leaves = subtree.leaves;
            leaf.children = subtree.children;
        } else {
            let leaf = leaf_parent.children.pop().unwrap();
            let leaf = AttrTreeLeaf::from_attr_tree_branch(leaf, value.syntax().clone());
            leaves.push(leaf);
            leaf_parent.leaves.push(leaves.len() - 1);
        }
    }

    fn from_attr_path(attr_path: Attrpath, before_empty_line: bool) -> Self {
        let mut root = Self::new(None, String::new(), before_empty_line);
        let mut parent = &mut root;

        for attr in attr_path.attrs() {
            let key = match &attr {
                Attr::Ident(x) => Some(x.to_string()),
                Attr::Dynamic(_) => None,
                Attr::Str(x) => x.parts().try_fold(String::new(), |acc, x| match x {
                    InterpolPart::Literal(x) => Some(acc + x.syntax().text()),
                    InterpolPart::Interpolation(_) => None,
                }),
            };
            let node = Self::new(key, attr.to_string(), before_empty_line);
            parent.children.push(node);

            parent = &mut parent.children[0];
        }

        root.children.into_iter().next().unwrap()
    }

    fn normalize(&mut self) {
        let mut i = 0;
        while i < self.children.len() {
            let key = self.children[i].key.clone();
            if key.is_none() {
                i += 1;
                continue;
            }

            let children: Vec<_> = self
                .children
                .extract_if(i + 1.., |x| x.key == key)
                .collect();
            for x in children {
                self.children[i].leaves.extend(x.leaves);
                self.children[i].children.extend(x.children);
            }
            self.children[i].normalize();

            i += 1;
        }
    }

    fn format(&mut self, leaves: &mut Vec<AttrTreeLeaf>) {
        for x in self.children.iter_mut() {
            Self::format_layer(leaves, vec![x], MAX_ATTRS_LEAVES);
        }
    }

    fn format_layer(
        leaves: &mut Vec<AttrTreeLeaf>,
        mut layer: Vec<&mut Self>,
        mut max_leaves: usize,
    ) {
        if layer.is_empty() {
            return;
        }

        let mut next_layer_size: usize = layer
            .iter()
            .map(|x| x.leaves.len() + x.children.len())
            .sum();

        while next_layer_size > max_leaves {
            let node = layer
                .iter_mut()
                .filter(|x| x.inline)
                .max_by_key(|x| x.leaves.len() + x.children.len())
                .unwrap();

            max_leaves -= 1;
            next_layer_size -= node.leaves.len() + node.children.len();

            node.inline = false;

            let mut node_leaves = Vec::new();
            node.collect_unsorted_leaves(&mut node_leaves);
            node_leaves.sort_unstable();

            let mut section_start = node_leaves[0];
            let mut first_section_end = None;
            let mut has_multiple_sections = false;

            for (current, next) in (0..node_leaves.len()).zip(1..node_leaves.len()) {
                let (current, next) = (node_leaves[current], node_leaves[next]);

                if current + 1 == next {
                    continue;
                }

                if first_section_end.is_none() {
                    first_section_end = Some(current);
                }

                if section_start != node_leaves[0] && leaves[current].before_empty_line {
                    leaves[section_start - 1].before_empty_line = true;
                }

                if (current..next).any(|i| leaves[i].before_empty_line) {
                    leaves[current].before_empty_line = true;
                    has_multiple_sections = true;
                }

                section_start = next;
            }

            if section_start != node_leaves[0]
                && leaves[*node_leaves.last().unwrap()].before_empty_line
            {
                leaves[section_start - 1].before_empty_line = true;
            }

            leaves[*node_leaves.last().unwrap()].before_empty_line = false;

            let first_section_end =
                first_section_end.unwrap_or_else(|| *node_leaves.last().unwrap());
            if has_multiple_sections || leaves[first_section_end].before_empty_line {
                node.before_empty_line = true;
            }

            node.format(leaves);
        }

        max_leaves -= layer
            .iter()
            .filter(|x| x.inline)
            .map(|x| x.leaves.len())
            .sum::<usize>();
        let next_layer = layer
            .into_iter()
            .filter(|x| x.inline)
            .flat_map(|x| x.children.iter_mut())
            .collect();
        Self::format_layer(leaves, next_layer, max_leaves);
    }

    fn collect_unsorted_leaves(&self, leaves: &mut Vec<usize>) {
        leaves.extend(self.leaves.iter());
        for x in self.children.iter() {
            x.collect_unsorted_leaves(leaves);
        }
    }

    fn print_attr_set(
        &self,
        leaves: &Vec<AttrTreeLeaf>,
        file: &str,
        format: AttrSetFormat,
    ) -> (String, usize) {
        let mut attrs: Vec<_> = self
            .children
            .iter()
            .flat_map(|x| x.print_attr(leaves, file, &format))
            .chain(self.leaves.iter().map(|&i| leaves[i].print(file, i)))
            .collect();
        attrs.sort_unstable_by_key(|x| x.1);

        let position = attrs[0].1;
        let text = match &format {
            AttrSetFormat::Inline(recursive, indentation) => {
                let body = attrs
                    .into_iter()
                    .map(|x| x.0)
                    .reduce(|acc, x| acc + " " + &x)
                    .unwrap();
                let rec = if *recursive { "rec " } else { "" };
                format!("{rec}{{{indentation}{body}{indentation}}}")
            }
            AttrSetFormat::Multiline(recursive, start_indentation, indentation) => {
                let body: String = attrs
                    .into_iter()
                    .map(|(x, _, before_empty_line)| {
                        let suffix = if before_empty_line { "\n" } else { "" };
                        format!("\n{start_indentation}{indentation}{x}{suffix}")
                    })
                    .collect();
                let rec = if *recursive { "rec " } else { "" };
                format!("{rec}{{{body}\n{start_indentation}}}")
            }
        };
        (text, position)
    }

    fn print_attr(
        &self,
        leaves: &Vec<AttrTreeLeaf>,
        file: &str,
        format: &AttrSetFormat,
    ) -> Vec<(String, usize, bool)> {
        if !self.inline {
            let (value, i) = self.print_attr_set(leaves, file, format.subset_format());
            let text = format!("{} = {value};", self.key_text);
            return vec![(text, i, self.before_empty_line)];
        }

        self.children
            .iter()
            .flat_map(|x| x.print_attr(leaves, file, &format))
            .chain(self.leaves.iter().map(|&i| leaves[i].print(file, i)))
            .map(|x| (format!("{}.{}", self.key_text, x.0), x.1, x.2))
            .collect()
    }
}

impl AttrTreeLeaf {
    fn from_attr_tree_branch(branch: AttrTreeBranch, value: SyntaxNode) -> Self {
        Self {
            key_text: branch.key_text,
            value,
            before_empty_line: branch.before_empty_line,
        }
    }

    fn print(&self, file: &str, position: usize) -> (String, usize, bool) {
        let value = format_node(file, self.value.clone());
        let text = format!("{} = {value};", self.key_text);
        (text, position, self.before_empty_line)
    }
}

#[derive(Debug)]
enum AttrSetFormat {
    Inline(bool, String),
    Multiline(bool, String, String),
}

impl AttrSetFormat {
    fn new(file: &str, attr_set: AttrSet) -> Self {
        let recursive = attr_set.rec_token().is_some();

        let whitespace = attr_set
            .syntax()
            .children_with_tokens()
            .skip_while(|x| x.kind() != SyntaxKind::TOKEN_L_BRACE)
            .nth(1)
            .unwrap();
        let whitespace = if whitespace.kind() == SyntaxKind::TOKEN_WHITESPACE {
            whitespace.to_string()
        } else {
            String::new()
        };

        match whitespace.rfind('\n') {
            None => AttrSetFormat::Inline(recursive, whitespace),
            Some(i) => {
                let start_indentation = file[..attr_set.syntax().text_range().start().into()]
                    .rsplit('\n')
                    .next()
                    .unwrap()
                    .split(|x: char| !x.is_whitespace())
                    .next()
                    .unwrap()
                    .to_string();
                let indentation = whitespace[i + 1..]
                    .strip_prefix(&start_indentation)
                    .unwrap_or("  ")
                    .to_string();
                AttrSetFormat::Multiline(recursive, start_indentation, indentation)
            }
        }
    }

    fn subset_format(&self) -> Self {
        match self {
            AttrSetFormat::Inline(_, x) => AttrSetFormat::Inline(false, x.clone()),
            AttrSetFormat::Multiline(_, start_indentation, indentation) => {
                let start_indentation = start_indentation.clone() + indentation;
                AttrSetFormat::Multiline(false, start_indentation, indentation.clone())
            }
        }
    }
}
