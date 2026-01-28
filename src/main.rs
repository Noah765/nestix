use std::fs;

use rnix::{
    NodeOrToken, Root, SyntaxElement, SyntaxNode,
    ast::{
        AstToken, Attr, AttrSet, Attrpath, AttrpathValue, Expr, HasEntry, InterpolPart, Whitespace,
    },
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
            let mut tree = AttrTreeNode::from_attr_set(x.clone(), &mut 0);
            tree.normalize();
            tree.format();
            tree.print(file, AttrSetFormat::new(file, x))
        }
    }
}

#[derive(Debug)]
enum AttrSetFormat {
    Inline(String),
    Multiline(String, String),
}

impl AttrSetFormat {
    fn new(file: &str, attr_set: AttrSet) -> Self {
        let whitespace = attr_set.syntax().children_with_tokens().nth(1).unwrap();
        let whitespace = match SyntaxElement::into_token(whitespace).and_then(Whitespace::cast) {
            None => String::new(),
            Some(x) => x.syntax().to_string(),
        };

        match whitespace.rfind('\n') {
            None => AttrSetFormat::Inline(whitespace),
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
                AttrSetFormat::Multiline(start_indentation, indentation)
            }
        }
    }

    fn with_increased_indentation(&self) -> Self {
        match self {
            AttrSetFormat::Inline(indentation) => AttrSetFormat::Inline(indentation.clone()),
            AttrSetFormat::Multiline(start_indentation, indentation) => AttrSetFormat::Multiline(
                start_indentation.clone() + indentation,
                indentation.clone(),
            ),
        }
    }
}

#[derive(Debug)]
struct AttrTreeNode {
    key: Option<String>,
    key_text: String,
    value: Option<SyntaxNode>,
    position: u32,
    inline: bool,
    children: Vec<AttrTreeNode>,
}

impl AttrTreeNode {
    fn new(key: Option<String>, key_text: String, position: u32) -> Self {
        Self {
            key,
            key_text,
            value: None,
            position,
            inline: true,
            children: Vec::new(),
        }
    }

    fn from_attr_set(attr_set: AttrSet, position: &mut u32) -> Self {
        let mut root = Self::new(None, String::new(), *position);
        for attr_path_value in attr_set.attrpath_values() {
            root.children
                .push(Self::from_attr_path_value(attr_path_value, position));
        }
        root
    }

    fn from_attr_path_value(attr_path_value: AttrpathValue, position: &mut u32) -> Self {
        let mut tree = Self::from_attr_path(attr_path_value.attrpath().unwrap(), position);
        let mut leaf = &mut tree;
        while !leaf.children.is_empty() {
            leaf = &mut leaf.children[0];
        }

        let mut value = attr_path_value.value().unwrap();
        while let Expr::Paren(x) = value {
            value = x.expr().unwrap();
        }

        if let Expr::AttrSet(attr_set) = value {
            leaf.children = Self::from_attr_set(attr_set, position).children;
        } else {
            leaf.value = Some(value.syntax().clone());
        }

        tree
    }

    fn from_attr_path(attr_path: Attrpath, position: &mut u32) -> Self {
        let mut root = Self::new(None, String::new(), *position);
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

            let node = Self::new(key, attr.syntax().to_string(), *position);
            parent.children.push(node);

            parent = &mut parent.children[0];
        }

        *position += 1;

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

            let collected_children: Vec<_> = self
                .children
                .extract_if(i + 1.., |x| x.key == key)
                .flat_map(|x| x.children)
                .collect();
            self.children[i].children.extend(collected_children);

            self.children[i].normalize();

            i += 1;
        }
    }

    fn format(&mut self) {
        for x in self.children.iter_mut() {
            Self::format_layer(vec![x], MAX_ATTRS_LEAVES);
        }
    }

    fn format_layer(mut layer: Vec<&mut Self>, mut max_leaves: usize) {
        if layer.is_empty() {
            return;
        }

        max_leaves -= layer.iter().filter(|x| x.children.is_empty()).count();

        let mut next_layer_size: usize = layer.iter().map(|x| x.children.len()).sum();
        while next_layer_size > max_leaves {
            let x = layer
                .iter_mut()
                .filter(|x| x.inline)
                .max_by_key(|x| x.children.len())
                .unwrap();

            max_leaves -= 1;
            next_layer_size -= x.children.len();

            x.inline = false;
            x.format();
        }

        let next_layer = layer
            .into_iter()
            .filter(|x| x.inline)
            .flat_map(|x| x.children.iter_mut())
            .collect();
        Self::format_layer(next_layer, max_leaves);
    }

    fn print(&self, file: &str, format: AttrSetFormat) -> String {
        let mut attrs: Vec<_> = self
            .children
            .iter()
            .flat_map(|x| x.print_node(file, &format))
            .collect();
        attrs.sort_unstable();

        match &format {
            AttrSetFormat::Inline(indentation) => {
                let body = attrs
                    .into_iter()
                    .map(|(_, x)| x)
                    .reduce(|acc, x| acc + " " + &x)
                    .unwrap();
                format!("{{{indentation}{body}{indentation}}}")
            }
            AttrSetFormat::Multiline(start_indentation, indentation) => {
                let body: String = attrs
                    .into_iter()
                    .map(|(_, x)| format!("\n{start_indentation}{indentation}{x}"))
                    .collect();
                format!("{{{body}\n{start_indentation}}}")
            }
        }
    }

    fn print_node(&self, file: &str, format: &AttrSetFormat) -> Vec<(u32, String)> {
        if self.children.is_empty() {
            let value = format_node(file, self.value.clone().unwrap());
            vec![(self.position, format!("{} = {value};", self.key_text))]
        } else if self.inline {
            self.children
                .iter()
                .flat_map(|x| x.print_node(file, format))
                .map(|(position, x)| (position, format!("{}.{x}", self.key_text)))
                .collect()
        } else {
            let body = self.print(file, format.with_increased_indentation());
            vec![(self.position, format!("{} = {body};", self.key_text))]
        }
    }
}
