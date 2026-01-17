use std::{collections::HashSet, fs};

use rnix::{
    Root, SyntaxElement, TextSize,
    ast::{
        AstToken, Attr, AttrSet, Attrpath, AttrpathValue, Expr, HasEntry, InterpolPart, Whitespace,
    },
};
use rowan::ast::AstNode;

const MAX_ATTRS_LEAVES: usize = 2;

fn main() {
    let file = fs::read_to_string("flake.nix").unwrap();

    let mut visited_attr_sets = HashSet::new();

    for attr_set in Root::parse(&file)
        .syntax()
        .descendants()
        .filter_map(AttrSet::cast)
    {
        if visited_attr_sets.contains(&attr_set.syntax().text_range().start()) {
            continue;
        }

        let mut tree = AttrTreeNode::from_attr_set(attr_set.clone(), &mut visited_attr_sets);
        tree.normalize();
        tree.format();

        println!("{}", tree.print(AttrSetFormat::new(&file, attr_set)));
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
    text: String,
    inline: bool,
    children: Vec<AttrTreeNode>,
}

impl AttrTreeNode {
    fn new(key: Option<String>, text: String) -> Self {
        Self {
            key,
            text,
            inline: true,
            children: Vec::new(),
        }
    }

    fn from_attr_set(attr_set: AttrSet, visited_attr_sets: &mut HashSet<TextSize>) -> Self {
        visited_attr_sets.insert(attr_set.syntax().text_range().start());

        let mut root = Self::new(None, String::new());
        for attr_path_value in attr_set.attrpath_values() {
            root.children.push(Self::from_attr_path_value(
                attr_path_value,
                visited_attr_sets,
            ));
        }

        root
    }

    fn from_attr_path_value(
        attr_path_value: AttrpathValue,
        visited_attr_sets: &mut HashSet<TextSize>,
    ) -> Self {
        let mut tree = Self::from_attr_path(attr_path_value.attrpath().unwrap());
        let mut leaf = &mut tree;
        while !leaf.children.is_empty() {
            leaf = &mut leaf.children[0];
        }

        let mut value = attr_path_value.value().unwrap();
        while let Expr::Paren(x) = value {
            value = x.expr().unwrap();
        }

        if let Expr::AttrSet(attr_set) = value {
            leaf.children = Self::from_attr_set(attr_set, visited_attr_sets).children;
        } else {
            leaf.text += &format!(" = {};", value.syntax());
        }

        tree
    }

    fn from_attr_path(attr_path: Attrpath) -> Self {
        let mut root = Self::new(None, String::new());
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

            let node = Self::new(key, attr.syntax().to_string());
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

    fn print(&self, format: AttrSetFormat) -> String {
        match &format {
            AttrSetFormat::Inline(indentation) => {
                let body = self
                    .children
                    .iter()
                    .flat_map(|x| x.print_node(&format))
                    .reduce(|acc, x| acc + " " + &x)
                    .unwrap();
                format!("{{{indentation}{body}{indentation}}}")
            }
            AttrSetFormat::Multiline(start_indentation, indentation) => {
                let body: String = self
                    .children
                    .iter()
                    .flat_map(|x| x.print_node(&format))
                    .map(|x| format!("\n{start_indentation}{indentation}{x}"))
                    .collect();
                format!("{{{body}\n{start_indentation}}}")
            }
        }
    }

    fn print_node(&self, format: &AttrSetFormat) -> Vec<String> {
        if self.children.is_empty() {
            vec![self.text.clone()]
        } else if self.inline {
            self.children
                .iter()
                .flat_map(|x| x.print_node(format))
                .map(|x| format!("{}.{x}", self.text))
                .collect()
        } else {
            vec![format!(
                "{} = {};",
                self.text,
                self.print(format.with_increased_indentation())
            )]
        }
    }
}
