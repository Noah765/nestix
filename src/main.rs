use std::{collections::HashSet, fs};

use rnix::{
    Root, TextSize,
    ast::{AstToken, Attr, AttrSet, Attrpath, AttrpathValue, Expr, HasEntry, InterpolPart},
};
use rowan::ast::AstNode;

const MAX_ATTRS_LEAVES: usize = 2;

fn main() {
    let content = fs::read_to_string("flake.nix").unwrap();

    let mut visited_attr_sets = HashSet::new();

    for attr_set in Root::parse(&content)
        .syntax()
        .descendants()
        .filter_map(AttrSet::cast)
    {
        if visited_attr_sets.contains(&attr_set.syntax().text_range().start()) {
            continue;
        }

        let mut tree = AttrTreeNode::from_attr_set(attr_set, &mut visited_attr_sets);
        tree.format();

        println!("{tree:#?}");
    }
}

#[derive(Debug)]
struct AttrTreeNode {
    text: Option<String>,
    inline: bool,
    children: Vec<AttrTreeNode>,
}

impl AttrTreeNode {
    fn new(text: Option<String>) -> Self {
        Self {
            text,
            inline: true,
            children: Vec::new(),
        }
    }

    fn from_attr_set(attr_set: AttrSet, visited_attr_sets: &mut HashSet<TextSize>) -> Self {
        visited_attr_sets.insert(attr_set.syntax().text_range().start());

        let mut root = Self::new(None);
        for attr_path_value in attr_set.attrpath_values() {
            root.children.push(Self::from_attr_path_value(
                attr_path_value,
                visited_attr_sets,
            ));
        }
        root.normalize_tree();

        root
    }

    fn from_attr_path_value(
        attr_path_value: AttrpathValue,
        visited_attr_sets: &mut HashSet<TextSize>,
    ) -> Self {
        let mut tree = Self::from_attr_path(attr_path_value.attrpath().unwrap());

        let mut value = attr_path_value.value().unwrap();
        while let Expr::Paren(x) = value {
            value = x.expr().unwrap();
        }

        if let Expr::AttrSet(attr_set) = value {
            let mut x = &mut tree;
            while !x.children.is_empty() {
                x = &mut x.children[0];
            }
            x.children = Self::from_attr_set(attr_set, visited_attr_sets).children;
        }

        tree
    }

    fn from_attr_path(attr_path: Attrpath) -> Self {
        let mut root = Self::new(None);
        let mut parent = &mut root;

        for attr in attr_path.attrs() {
            let text = match attr {
                Attr::Ident(x) => Some(x.to_string()),
                Attr::Dynamic(_) => None,
                Attr::Str(x) => x.parts().try_fold(String::new(), |acc, x| match x {
                    InterpolPart::Literal(x) => Some(acc + x.syntax().text()),
                    InterpolPart::Interpolation(_) => None,
                }),
            };

            parent.children.push(Self::new(text));
            parent = &mut parent.children[0];
        }

        root.children.into_iter().next().unwrap()
    }

    fn normalize_tree(&mut self) {
        let mut i = 0;
        while i < self.children.len() {
            let text = self.children[i].text.clone();

            if text.is_none() {
                i += 1;
                continue;
            }

            let collected_children: Vec<_> = self
                .children
                .extract_if(i + 1.., |x| x.text == text)
                .flat_map(|x| x.children)
                .collect();
            self.children[i].children.extend(collected_children);

            self.children[i].normalize_tree();

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
}
