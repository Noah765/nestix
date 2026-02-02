use std::fs;

use rnix::{
    NodeOrToken, Root, SyntaxKind, SyntaxNode,
    ast::{AstToken, Attr, AttrSet, Attrpath, AttrpathValue, Expr, HasEntry, InterpolPart},
};
use rowan::ast::AstNode;

const MAX_ATTRS_LEAVES: usize = 2;

fn main() {
    let file = fs::read_to_string("flake.nix").unwrap();
    let formatted_file = format_node(Root::parse(&file).syntax(), &file);
    println!("{formatted_file}");
}

fn format_node(node: SyntaxNode, file: &str) -> String {
    match AttrSet::cast(node.clone()) {
        Some(x) if x.attrpath_values().next().is_some() => {
            let mut tree = AttrTree::new(x.clone());
            tree.normalize();
            tree.format();
            tree.print(file)
        }
        _ => node
            .children_with_tokens()
            .fold(String::new(), |acc, x| match x {
                NodeOrToken::Node(x) => acc + &format_node(x, file),
                NodeOrToken::Token(x) => acc + x.text(),
            }),
    }
}

#[derive(Debug)]
struct AttrTree {
    attr_set: AttrSet,
    recursive: bool,
    root: Branch,
    leaves: Vec<Leaf>,
}

#[derive(Debug)]
struct Branch {
    key: Option<String>,
    key_text: String,
    inline: bool,
    before_empty_line: bool,
    leaves: Vec<usize>,
    children: Vec<Branch>,
}

#[derive(Debug)]
struct Leaf {
    key_text: String,
    value: Expr,
    before_empty_line: bool,
}

#[derive(Debug)]
enum Format {
    Inline(String),
    Multiline(String, String),
}

impl AttrTree {
    fn new(attr_set: AttrSet) -> Self {
        let mut leaves = Vec::new();
        let recursive = attr_set.rec_token().is_some();
        let root = Branch::from_attr_set(attr_set.clone(), &mut leaves);
        Self {
            attr_set,
            recursive,
            root,
            leaves,
        }
    }

    fn normalize(&mut self) {
        self.root.normalize();
    }

    fn format(&mut self) {
        self.root.format_attr_set(&mut self.leaves);
    }

    fn print(&self, file: &str) -> String {
        let format = Format::new(self.attr_set.clone(), file);
        self.root
            .print_attr_set(self.recursive, &self.leaves, format, file)
            .0
    }
}

impl Branch {
    fn new(key: Option<String>, key_text: String, inline: bool) -> Self {
        Self {
            key,
            key_text,
            inline,
            before_empty_line: false,
            leaves: Vec::new(),
            children: Vec::new(),
        }
    }

    fn from_attr_set(attr_set: AttrSet, leaves: &mut Vec<Leaf>) -> Self {
        let mut root = Self::new(None, String::new(), false);

        let mut elements = attr_set
            .syntax()
            .children_with_tokens()
            .skip_while(|x| matches!(x, NodeOrToken::Token(_)))
            .peekable();

        while let Some(NodeOrToken::Node(attr_path_value)) = elements.next() {
            let attr_path_value = AttrpathValue::cast(attr_path_value).unwrap();

            let before_empty_line = match elements.peek() {
                None | Some(NodeOrToken::Node(_)) => false,
                Some(NodeOrToken::Token(x)) => x.text().matches('\n').count() >= 2,
            };
            if let Some(NodeOrToken::Token(_)) = elements.peek() {
                elements.next();
            }

            root.add_attr_path_value(attr_path_value, before_empty_line, leaves);
        }

        root
    }

    fn add_attr_path_value(
        &mut self,
        attr_path_value: AttrpathValue,
        before_empty_line: bool,
        leaves: &mut Vec<Leaf>,
    ) {
        let node = Self::from_attr_path(attr_path_value.attrpath().unwrap());
        self.children.push(node);

        let mut leaf_parent = self;
        while !leaf_parent.children.last().unwrap().children.is_empty() {
            leaf_parent = leaf_parent.children.last_mut().unwrap();
        }
        leaf_parent.children.last_mut().unwrap().before_empty_line = before_empty_line;

        let mut value = attr_path_value.value().unwrap();
        while let Expr::Paren(x) = value {
            value = x.expr().unwrap();
        }

        if let Expr::AttrSet(attr_set) = value.clone()
            && attr_set.rec_token().is_none()
            && attr_set.attrpath_values().next().is_some()
        {
            let subtree = Self::from_attr_set(attr_set, leaves);
            let leaf = leaf_parent.children.last_mut().unwrap();
            leaf.inline = false;
            leaf.leaves = subtree.leaves;
            leaf.children = subtree.children;
        } else {
            let leaf = leaf_parent.children.pop().unwrap();
            let leaf = Leaf::from_attr_tree_branch(leaf, value);
            leaves.push(leaf);
            leaf_parent.leaves.push(leaves.len() - 1);
        }
    }

    fn from_attr_path(attr_path: Attrpath) -> Self {
        let mut root = Self::new(None, String::new(), true);
        let mut parent = &mut root;

        for attr in attr_path.attrs() {
            let key = match attr.clone() {
                Attr::Ident(x) => Some(x.to_string()),
                Attr::Dynamic(_) => None,
                Attr::Str(x) => x.parts().try_fold(String::new(), |acc, x| match x {
                    InterpolPart::Literal(x) => Some(acc + x.syntax().text()),
                    InterpolPart::Interpolation(_) => None,
                }),
            };
            let node = Self::new(key, attr.to_string(), true);
            parent.children.push(node);

            parent = &mut parent.children[0];
        }

        root.children.into_iter().next().unwrap()
    }

    fn normalize(&mut self) {
        let mut i = 0;
        while i < self.children.len() {
            let branch = &mut self.children[i];
            branch.inline = true;

            let key = branch.key.clone();
            if key.is_none() {
                i += 1;
                continue;
            }

            let children: Vec<_> = self
                .children
                .extract_if(i + 1.., |x| x.key == key)
                .collect();
            let branch = &mut self.children[i];
            for x in children {
                branch.leaves.extend(x.leaves);
                branch.children.extend(x.children);
            }

            branch.normalize();

            i += 1;
        }
    }

    fn format_attr_set(&mut self, leaves: &mut Vec<Leaf>) {
        for x in self.children.iter_mut() {
            Self::format_layer(vec![x], MAX_ATTRS_LEAVES, leaves);
        }
    }

    fn format_layer(mut layer: Vec<&mut Self>, mut max_leaves: usize, leaves: &mut Vec<Leaf>) {
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
            node.format_attr_set_whitespace(leaves);
            node.format_attr_set(leaves);
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
        Self::format_layer(next_layer, max_leaves, leaves);
    }

    fn format_attr_set_whitespace(&mut self, leaves: &mut Vec<Leaf>) {
        let mut node_leaves = Vec::new();
        self.collect_unsorted_leaves(&mut node_leaves);
        node_leaves.sort_unstable();

        let mut section_start = node_leaves[0];
        let mut first_section_end = None;
        let mut multiple_separated_sections = false;

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
                multiple_separated_sections = true;
            }

            section_start = next;
        }

        if section_start != node_leaves[0] && leaves[*node_leaves.last().unwrap()].before_empty_line
        {
            leaves[section_start - 1].before_empty_line = true;
        }

        leaves[*node_leaves.last().unwrap()].before_empty_line = false;

        let first_section_end = first_section_end.unwrap_or_else(|| *node_leaves.last().unwrap());
        if multiple_separated_sections || leaves[first_section_end].before_empty_line {
            self.before_empty_line = true;
        }
    }

    fn collect_unsorted_leaves(&self, leaves: &mut Vec<usize>) {
        leaves.extend(self.leaves.iter());
        for x in self.children.iter() {
            x.collect_unsorted_leaves(leaves);
        }
    }

    fn print_attr_set(
        &self,
        recursive: bool,
        leaves: &Vec<Leaf>,
        format: Format,
        file: &str,
    ) -> (String, usize) {
        let rec = if recursive { "rec " } else { "" };

        let mut attrs: Vec<_> = self
            .children
            .iter()
            .flat_map(|x| x.print_attr(leaves, &format, file))
            .chain(self.leaves.iter().map(|&i| leaves[i].print(i, file)))
            .collect();
        attrs.sort_unstable_by_key(|x| x.2);

        let position = attrs.first().map(|x| x.2).unwrap_or(0);
        let text = match &format {
            Format::Inline(indentation) => {
                let body = attrs
                    .into_iter()
                    .map(|x| x.0)
                    .reduce(|acc, x| acc + " " + &x)
                    .unwrap_or_else(String::new);
                format!("{rec}{{{indentation}{body}{indentation}}}")
            }
            Format::Multiline(start_indentation, indentation) => {
                let body: String = attrs
                    .into_iter()
                    .map(|(x, before_empty_line, _)| {
                        let suffix = if before_empty_line { "\n" } else { "" };
                        format!("\n{start_indentation}{indentation}{x}{suffix}")
                    })
                    .collect();
                format!("{rec}{{{body}\n{start_indentation}}}")
            }
        };
        (text, position)
    }

    fn print_attr(
        &self,
        leaves: &Vec<Leaf>,
        format: &Format,
        file: &str,
    ) -> Vec<(String, bool, usize)> {
        if !self.inline {
            let (value, i) = self.print_attr_set(false, leaves, format.subset_format(), file);
            let text = format!("{} = {value};", self.key_text);
            return vec![(text, self.before_empty_line, i)];
        }

        self.children
            .iter()
            .flat_map(|x| x.print_attr(leaves, &format, file))
            .chain(self.leaves.iter().map(|&i| leaves[i].print(i, file)))
            .map(|x| (format!("{}.{}", self.key_text, x.0), x.1, x.2))
            .collect()
    }
}

impl Leaf {
    fn from_attr_tree_branch(branch: Branch, value: Expr) -> Self {
        Self {
            key_text: branch.key_text,
            value,
            before_empty_line: branch.before_empty_line,
        }
    }

    fn print(&self, position: usize, file: &str) -> (String, bool, usize) {
        let value = format_node(self.value.syntax().clone(), file);
        let text = format!("{} = {value};", self.key_text);
        (text, self.before_empty_line, position)
    }
}

impl Format {
    fn new(attr_set: AttrSet, file: &str) -> Self {
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
            None => Format::Inline(whitespace),
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
                Format::Multiline(start_indentation, indentation)
            }
        }
    }

    fn subset_format(&self) -> Self {
        match self {
            Format::Inline(x) => Format::Inline(x.clone()),
            Format::Multiline(start_indentation, indentation) => {
                let start_indentation = start_indentation.clone() + indentation;
                Format::Multiline(start_indentation, indentation.clone())
            }
        }
    }
}
