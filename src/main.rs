use std::{
    fmt::{self, Debug, Formatter},
    fs, iter,
};

use rnix::{
    NodeOrToken, Root, SyntaxKind, SyntaxNode,
    ast::{
        AstToken, Attr, AttrSet, Attrpath, AttrpathValue, Comment, Expr, InterpolPart, Whitespace,
    },
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
        Some(x) if !is_attr_set_empty(x.clone()) => {
            let mut tree = AttrTree::new(x, file);
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

fn is_attr_set_empty(attr_set: AttrSet) -> bool {
    let first_element = attr_set
        .syntax()
        .children_with_tokens()
        .skip_while(|x| x.kind() != SyntaxKind::TOKEN_L_BRACE)
        .skip(1)
        .skip_while(|x| x.kind() == SyntaxKind::TOKEN_WHITESPACE)
        .next();
    first_element.unwrap().kind() == SyntaxKind::TOKEN_R_BRACE
}

#[derive(Debug)]
struct AttrTree {
    recursive: bool,
    root: Branch,
    leaves: Vec<Leaf>,
}

struct Branch {
    key: Option<String>,
    key_text: String,
    inline: bool,
    format: Option<Format>,
    before_empty_line: bool,
    comments_above: Vec<usize>,
    leaves: Vec<usize>,
    children: Vec<Branch>,
}

enum Leaf {
    Node {
        key_text: String,
        value: Expr,
        comments_above: Vec<String>,
        comment_right: Option<String>,
        before_empty_line: bool,
    },
    Comment {
        lines: Vec<String>,
        before_empty_line: bool,
    },
}

#[derive(Clone, Debug)]
enum Format {
    Inline(String),
    Multiline(String, String),
}

impl AttrTree {
    fn new(attr_set: AttrSet, file: &str) -> Self {
        let mut leaves = Vec::new();
        let recursive = attr_set.rec_token().is_some();
        let root = Branch::from_attr_set(attr_set.clone(), &mut leaves, file);
        Self {
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
        let (x, _) = self.root.print_attr_set(self.recursive, &self.leaves, file);
        x
    }
}

impl Branch {
    fn new(key: Option<String>, key_text: String, inline: bool, format: Option<Format>) -> Self {
        Self {
            key,
            key_text,
            inline,
            comments_above: Vec::new(),
            format,
            before_empty_line: false,
            leaves: Vec::new(),
            children: Vec::new(),
        }
    }

    fn from_attr_set(attr_set: AttrSet, leaves: &mut Vec<Leaf>, file: &str) -> Self {
        let format = Format::new(attr_set.clone(), file);
        let inline = matches!(format, Format::Inline(_));
        let mut root = Self::new(None, String::new(), false, Some(format));

        let mut elements = attr_set
            .syntax()
            .children_with_tokens()
            .skip_while(|x| x.kind() != SyntaxKind::TOKEN_L_BRACE)
            .skip(1)
            .skip_while(|x| x.kind() == SyntaxKind::TOKEN_WHITESPACE)
            .peekable();

        while elements.peek().unwrap().kind() != SyntaxKind::TOKEN_R_BRACE {
            let mut comments = Vec::new();
            while let NodeOrToken::Token(x) = elements.peek().unwrap()
                && (x.kind() == SyntaxKind::TOKEN_WHITESPACE && x.text().matches('\n').count() <= 1
                    || x.kind() == SyntaxKind::TOKEN_COMMENT)
            {
                if let Some(x) = Comment::cast(x.clone()) {
                    comments.push(x);
                }
                elements.next();
            }

            if let NodeOrToken::Token(_) = elements.peek().unwrap() {
                let new_leaf_indices = leaves.len()..leaves.len() + comments.len();
                root.leaves.extend(new_leaf_indices);

                leaves.extend(comments.into_iter().map(Leaf::from_comment));
                if elements.peek().unwrap().kind() == SyntaxKind::TOKEN_WHITESPACE {
                    *leaves.last_mut().unwrap().before_empty_line_mut() = true;
                    elements.next();
                }

                continue;
            }

            let attr_path_value =
                AttrpathValue::cast(elements.next().unwrap().into_node().unwrap()).unwrap();

            let mut value = attr_path_value.value().unwrap();
            while let Expr::Paren(x) = value {
                value = x.expr().unwrap();
            }
            let comment_right = if inline || value.syntax().kind() == SyntaxKind::NODE_ATTR_SET {
                None
            } else {
                if let NodeOrToken::Token(x) = elements.peek().unwrap()
                    && x.kind() == SyntaxKind::TOKEN_WHITESPACE
                    && !x.text().contains('\n')
                {
                    elements.next();
                }
                let comment = elements.peek().unwrap().clone().into_token();
                comment.and_then(Comment::cast).map(|x| {
                    elements.next();
                    String::from(x.syntax().text())
                })
            };

            let mut before_empty_line = false;
            if let NodeOrToken::Token(x) = elements.peek().unwrap()
                && x.kind() == SyntaxKind::TOKEN_WHITESPACE
            {
                before_empty_line = x.text().matches('\n').count() >= 2;
                elements.next();
            }

            root.add_attr_path_value(
                attr_path_value,
                value,
                comments,
                comment_right,
                before_empty_line,
                leaves,
                file,
            );
        }

        root
    }

    fn add_attr_path_value(
        &mut self,
        attr_path_value: AttrpathValue,
        value: Expr,
        comments_above: Vec<Comment>,
        comment_right: Option<String>,
        before_empty_line: bool,
        leaves: &mut Vec<Leaf>,
        file: &str,
    ) {
        let node = Self::from_attr_path(attr_path_value.attrpath().unwrap());
        self.children.push(node);

        let mut leaf_parent = self;
        while !leaf_parent.children.last().unwrap().children.is_empty() {
            leaf_parent = leaf_parent.children.last_mut().unwrap();
        }
        let leaf = leaf_parent.children.last_mut().unwrap();
        leaf.before_empty_line = before_empty_line;

        if let Expr::AttrSet(attr_set) = value.clone()
            && attr_set.rec_token().is_none()
            && !is_attr_set_empty(attr_set.clone())
        {
            leaf.comments_above = (leaves.len()..leaves.len() + comments_above.len()).collect();
            leaves.extend(comments_above.into_iter().map(Leaf::from_comment));
            let subtree = Self::from_attr_set(attr_set, leaves, file);
            leaf.inline = false;
            leaf.format = subtree.format;
            leaf.leaves = subtree.leaves;
            leaf.children = subtree.children;
        } else {
            let leaf = leaf_parent.children.pop().unwrap();
            let leaf = Leaf::from_attr_tree_branch(leaf, value, comments_above, comment_right);
            leaves.push(leaf);
            leaf_parent.leaves.push(leaves.len() - 1);
        }
    }

    fn from_attr_path(attr_path: Attrpath) -> Self {
        let mut root = Self::new(None, String::new(), true, None);
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
            let node = Self::new(key, attr.to_string(), true, None);
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
            if !children.is_empty() {
                branch.format = None;
            }
            for x in children {
                if !x.inline {
                    branch.before_empty_line = x.before_empty_line;
                }
                branch.comments_above.extend(x.comments_above);
                branch.leaves.extend(x.leaves);
                branch.children.extend(x.children);
            }

            branch.normalize();

            i += 1;
        }
    }

    fn format_attr_set(&mut self, leaves: &mut Vec<Leaf>) {
        for x in self.children.iter_mut() {
            let format = self.format.as_ref().unwrap().subset_format();
            Self::format_layer(vec![x], MAX_ATTRS_LEAVES, format, leaves);
        }
    }

    fn format_layer(
        mut layer: Vec<&mut Self>,
        mut max_leaves: usize,
        format: Format,
        leaves: &mut Vec<Leaf>,
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
            node.format.get_or_insert_with(|| format.clone());
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
        Self::format_layer(next_layer, max_leaves, format.subset_format(), leaves);
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

            if section_start != node_leaves[0] && leaves[current].before_empty_line() {
                *leaves[section_start - 1].before_empty_line_mut() = true;
            }

            if (current..next).any(|i| leaves[i].before_empty_line()) {
                *leaves[current].before_empty_line_mut() = true;
                multiple_separated_sections = true;
            }

            section_start = next;
        }

        if section_start != node_leaves[0]
            && leaves[*node_leaves.last().unwrap()].before_empty_line()
        {
            *leaves[section_start - 1].before_empty_line_mut() = true;
        }

        *leaves[*node_leaves.last().unwrap()].before_empty_line_mut() = false;

        let first_section_end = first_section_end.unwrap_or_else(|| *node_leaves.last().unwrap());
        if multiple_separated_sections || leaves[first_section_end].before_empty_line() {
            self.before_empty_line = true;
        }
    }

    fn collect_unsorted_leaves(&self, leaves: &mut Vec<usize>) {
        leaves.extend(self.leaves.iter());
        for x in self.children.iter() {
            x.collect_unsorted_leaves(leaves);
        }
    }

    fn print_attr_set(&self, recursive: bool, leaves: &Vec<Leaf>, file: &str) -> (String, usize) {
        let rec = if recursive { "rec " } else { "" };

        let mut elements: Vec<_> = self
            .children
            .iter()
            .flat_map(|x| x.print_attr(leaves, file))
            .chain(self.leaves.iter().map(|&i| leaves[i].print(i, file)))
            .collect();
        elements.sort_unstable_by_key(|x| x.3);

        let position = elements.first().map(|x| x.3).unwrap_or(0);
        let text = match self.format.as_ref().unwrap() {
            Format::Inline(indentation) => {
                let body = elements
                    .into_iter()
                    .flat_map(|x| x.0.into_iter().chain(x.1))
                    .reduce(|acc, x| acc + " " + &x)
                    .unwrap_or_else(String::new);
                format!("{rec}{{{indentation}{body}{indentation}}}")
            }
            Format::Multiline(start_indentation, indentation) => {
                let body: String = elements
                    .into_iter()
                    .map(|(comments, x, before_empty_line, _)| {
                        let text: String = comments
                            .into_iter()
                            .chain(x)
                            .map(|x| format!("{start_indentation}{indentation}{x}\n"))
                            .collect();
                        text + if before_empty_line { "\n" } else { "" }
                    })
                    .collect();
                format!("{rec}{{\n{body}{start_indentation}}}")
            }
        };
        (text, position)
    }

    fn print_attr(
        &self,
        leaves: &Vec<Leaf>,
        file: &str,
    ) -> Vec<(Vec<String>, Option<String>, bool, usize)> {
        if self.inline {
            self.comments_above
                .iter()
                .chain(self.leaves.iter())
                .map(|&i| leaves[i].print(i, file))
                .chain(
                    self.children
                        .iter()
                        .flat_map(|x| x.print_attr(leaves, file)),
                )
                .map(|x| (x.0, x.1.map(|x| format!("{}.{x}", self.key_text)), x.2, x.3))
                .collect()
        } else {
            let comments_above = self
                .comments_above
                .iter()
                .flat_map(|&i| leaves[i].print(i, file).0)
                .collect();
            let (value, position) = self.print_attr_set(false, leaves, file);
            let text = format!("{} = {value};", self.key_text);
            vec![(comments_above, Some(text), self.before_empty_line, position)]
        }
    }
}

impl Debug for Branch {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Branch")
            .field("key", &format_args!("{:?}", self.key))
            .field("key_text", &self.key_text)
            .field("inline", &self.inline)
            .field("format", &format_args!("{:?}", self.format))
            .field("before_empty_line", &self.before_empty_line)
            .field("comments_above", &format_args!("{:?}", self.comments_above))
            .field("leaves", &format_args!("{:?}", self.leaves))
            .field("children", &self.children)
            .finish()
    }
}

impl Leaf {
    fn from_attr_tree_branch(
        branch: Branch,
        value: Expr,
        comments_above: Vec<Comment>,
        comment_right: Option<String>,
    ) -> Self {
        Self::Node {
            key_text: branch.key_text,
            value,
            comments_above: comments_above
                .into_iter()
                .flat_map(Self::parse_comment)
                .collect(),
            comment_right,
            before_empty_line: branch.before_empty_line,
        }
    }

    fn from_comment(comment: Comment) -> Self {
        Self::Comment {
            lines: Self::parse_comment(comment),
            before_empty_line: false,
        }
    }

    fn parse_comment(comment: Comment) -> Vec<String> {
        let lines: Vec<_> = comment.syntax().text().lines().collect();
        if lines.len() == 1 {
            return lines.into_iter().map(String::from).collect();
        }

        let indentation = lines[1..lines.len() - 1]
            .into_iter()
            .map(|x| &x[..x.len() - x.trim_start().len()])
            .min_by_key(|x| x.len())
            .map_or_else(String::new, |x| String::from(x));
        lines[..lines.len() - 1]
            .into_iter()
            .map(|x| String::from(x.strip_prefix(&indentation).unwrap_or(x)))
            .chain(iter::once(String::from(lines.last().unwrap().trim_start())))
            .collect()
    }

    fn before_empty_line(&self) -> bool {
        match self {
            &Leaf::Node {
                before_empty_line, ..
            } => before_empty_line,
            &Leaf::Comment {
                before_empty_line, ..
            } => before_empty_line,
        }
    }

    fn before_empty_line_mut(&mut self) -> &mut bool {
        match self {
            Leaf::Node {
                before_empty_line, ..
            } => before_empty_line,
            Leaf::Comment {
                before_empty_line, ..
            } => before_empty_line,
        }
    }

    fn print(&self, position: usize, file: &str) -> (Vec<String>, Option<String>, bool, usize) {
        match self {
            Leaf::Node {
                key_text,
                value,
                comments_above,
                comment_right,
                before_empty_line,
            } => {
                let text = Some(format!(
                    "{key_text} = {};{}{}",
                    format_node(value.syntax().clone(), file),
                    if comment_right.is_some() { " " } else { "" },
                    comment_right.as_deref().unwrap_or(""),
                ));
                (comments_above.clone(), text, *before_empty_line, position)
            }
            Leaf::Comment {
                lines,
                before_empty_line,
            } => (lines.clone(), None, *before_empty_line, position),
        }
    }
}

impl Debug for Leaf {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Leaf::Node {
                key_text,
                value,
                comments_above,
                comment_right,
                before_empty_line,
            } => {
                let mut value = value.to_string();
                if value.len() >= 96 {
                    value.truncate(96);
                    value += "[..]";
                }
                let comments_above: &dyn Debug = if comments_above.len() > 1 {
                    comments_above
                } else {
                    &format_args!("{comments_above:?}")
                };
                f.debug_struct("Leaf::Node")
                    .field("key_text", key_text)
                    .field("value", &format_args!("Expr({value:?})"))
                    .field("comments_above", comments_above)
                    .field("comment_right", &format_args!("{comment_right:?}"))
                    .field("before_empty_line", before_empty_line)
                    .finish()
            }
            Leaf::Comment {
                lines,
                before_empty_line,
            } => {
                let lines: &dyn Debug = if lines.len() > 1 {
                    lines
                } else {
                    &format_args!("{lines:?}")
                };
                f.debug_struct("Leaf::Comment")
                    .field("lines", lines)
                    .field("before_empty_line", before_empty_line)
                    .finish()
            }
        }
    }
}

impl Format {
    fn new(attr_set: AttrSet, file: &str) -> Self {
        let line_breaking_whitespace = attr_set
            .syntax()
            .children_with_tokens()
            .filter_map(|x| NodeOrToken::into_token(x).and_then(Whitespace::cast))
            .find(|x| x.syntax().text().contains('\n'));

        match line_breaking_whitespace {
            None => Format::Inline(
                attr_set
                    .syntax()
                    .children_with_tokens()
                    .skip_while(|x| x.kind() != SyntaxKind::TOKEN_L_BRACE)
                    .nth(1)
                    .filter(|x| x.kind() == SyntaxKind::TOKEN_WHITESPACE)
                    .map_or_else(String::new, |x| x.to_string()),
            ),
            Some(x) => {
                let start_indentation = file[..attr_set.syntax().text_range().start().into()]
                    .rsplit('\n')
                    .next()
                    .unwrap()
                    .split(|x: char| !x.is_whitespace())
                    .next()
                    .unwrap()
                    .to_string();
                let indentation = x
                    .syntax()
                    .text()
                    .rsplit('\n')
                    .next()
                    .unwrap()
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
