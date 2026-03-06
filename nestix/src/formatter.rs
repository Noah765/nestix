use std::collections::HashMap;

use rnix::{
    NodeOrToken, ParseError, Root, SyntaxNode,
    ast::{AstToken, AttrSet, InterpolPart, Str, Whitespace},
};
use rowan::ast::AstNode;

use crate::attribute_set::AttributeSet;

#[derive(Clone, Debug)]
/// Formats Nix code into a string buffer.
///
/// Collects output while tracking the current indentation state.
pub struct Formatter {
    buffer: String,
    indentation_type: IndentationType,
    indentation_level: usize,
    syntax_tree_indentation_offset: isize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IndentationType {
    TwoSpaces,
    FourSpaces,
    Tabs,
}

impl Formatter {
    /// Creates a new `Formatter` with the given `indentation_type`.
    pub fn new(indentation_type: IndentationType) -> Self {
        Self {
            buffer: String::new(),
            indentation_type,
            indentation_level: 0,
            syntax_tree_indentation_offset: 0,
        }
    }

    /// Creates a child formatter with the same indentation state.
    pub fn child_formatter(&self) -> Self {
        Self {
            buffer: String::new(),
            indentation_type: self.indentation_type,
            indentation_level: self.indentation_level,
            syntax_tree_indentation_offset: self.syntax_tree_indentation_offset,
        }
    }

    /// Returns a newline plus the current indentation prefix.
    pub fn line_separator(&self) -> String {
        let mut result = match self.indentation_type {
            IndentationType::TwoSpaces => "  ".repeat(self.indentation_level),
            IndentationType::FourSpaces => "    ".repeat(self.indentation_level),
            IndentationType::Tabs => "\t".repeat(self.indentation_level),
        };
        result.insert(0, '\n');
        result
    }

    /// Appends `text` to the output buffer.
    pub fn write(&mut self, text: &str) {
        self.buffer += text;
    }

    /// Starts a new line using the current indentation prefix.
    pub fn open_line(&mut self) {
        self.write("\n");
        self.indent_with(match self.indentation_type {
            IndentationType::TwoSpaces => "  ",
            IndentationType::FourSpaces => "    ",
            IndentationType::Tabs => "\t",
        });
    }

    /// Writes `indentation_per_level` `self.indentation_level` times.
    fn indent_with(&mut self, indentation_per_level: &str) {
        for _ in 0..self.indentation_level {
            self.write(indentation_per_level);
        }
    }

    /// Increases the current indentation level by one.
    pub fn increase_indentation(&mut self) {
        self.indentation_level += 1;
    }

    /// Decreases the current indentation level by one, saturating at `0`.
    pub fn decrease_indentation(&mut self) {
        self.indentation_level = self.indentation_level.saturating_sub(1);
    }

    /// Sets `syntax_tree_indentation_offset` relative to
    /// `syntax_tree_indentation_level`.
    pub fn set_syntax_tree_indentation_offset(&mut self, syntax_tree_indentation_level: usize) {
        self.syntax_tree_indentation_offset =
            self.indentation_level as isize - syntax_tree_indentation_level as isize;
    }

    /// Resets `syntax_tree_indentation_offset` to `0`.
    pub fn reset_syntax_tree_indentation_offset(&mut self) {
        self.syntax_tree_indentation_offset = 0;
    }

    /// Consumes the formatter and returns the formatted output.
    pub fn into_string(self) -> String {
        self.buffer
    }

    /// Formats the Nix `expression` into a string.
    ///
    /// # Errors
    ///
    /// Returns an error if `expression` is not a valid Nix expression.
    pub fn format(expression: &str) -> Result<String, ParseError> {
        let tree = Root::parse(expression).ok()?;
        let mut formatter = Formatter::new(Self::guess_indentation_type(expression));
        formatter.format_node(tree.syntax().clone());
        Ok(formatter.into_string())
    }

    /// Formats `node` and writes the output into the buffer.
    pub fn format_node(&mut self, node: SyntaxNode) {
        if let Some(x) = AttrSet::cast(node.clone()) {
            let mut tree = AttributeSet::new(x, self.indentation_level);
            tree.format();
            tree.print(self);
            return;
        }
        if let Some(x) = Str::cast(node.clone())
            && node
                .children_with_tokens()
                .next()
                .and_then(|x| x.into_token())
                .is_some_and(|x| x.text() == "''")
        {
            self.format_indented_string(x);
            return;
        }

        for x in node.children_with_tokens() {
            match x {
                NodeOrToken::Node(x) => self.format_node(x),
                NodeOrToken::Token(x) => match Whitespace::cast(x.clone()) {
                    None => self.write(x.text()),
                    Some(x) => self.format_whitespace_token(x),
                },
            }
        }
    }

    /// Writes `token` with normalized indentation and updates the indentation
    /// level of this formatter.
    fn format_whitespace_token(&mut self, token: Whitespace) {
        let (last_line_index, last_line) = token
            .syntax()
            .text()
            .split('\n')
            .enumerate()
            .last()
            .expect("`split('\n')` should not be empty");

        if last_line_index == 0 {
            self.write(last_line);
            return;
        }

        let indentation = &last_line[..last_line.len() - last_line.trim_start().len()];

        let adjusted_level = self.guess_indentation_level(indentation) as isize
            + self.syntax_tree_indentation_offset;
        self.indentation_level = adjusted_level.max(0) as usize;

        for _ in 0..last_line_index - 1 {
            self.write("\n");
        }
        self.open_line();
    }

    /// Writes the indented string `node` with normalized indentation.
    fn format_indented_string(&mut self, node: Str) {
        let content = node.to_string();
        if !content.contains('\n') {
            self.write(&content);
            return;
        }

        self.increase_indentation();
        self.write("''");

        let prefix_space_count = content[2..content.len() - 2]
            .lines()
            .filter(|x| x.chars().any(|x| x != ' '))
            .map(|x| x.len() - x.trim_start_matches(' ').len())
            .min()
            .unwrap_or(0);

        let indentation_per_level = match self.indentation_type {
            IndentationType::TwoSpaces => "  ",
            IndentationType::FourSpaces | IndentationType::Tabs => "    ",
        };

        let mut parts = node.parts().peekable();
        let mut is_first = true;
        while let Some(part) = parts.next() {
            match part {
                InterpolPart::Literal(x) => {
                    let mut lines = x.syntax().text().split('\n');
                    let first_line = lines.next().expect("`lines` should not be empty");
                    let last_line = lines.next_back();

                    if is_first && first_line.len() > prefix_space_count {
                        self.indent_with(indentation_per_level);
                        self.write(&first_line[prefix_space_count..]);
                    } else {
                        self.write(first_line);
                    }

                    for x in lines {
                        self.write("\n");
                        if x.len() <= prefix_space_count {
                            continue;
                        }
                        self.indent_with(indentation_per_level);
                        self.write(&x[prefix_space_count..]);
                    }

                    let Some(last_line) = last_line else {
                        is_first = false;
                        continue;
                    };
                    self.write("\n");
                    if last_line.len() <= prefix_space_count && parts.peek().is_none() {
                        self.decrease_indentation();
                        self.indent_with(indentation_per_level);
                        self.increase_indentation();
                    } else {
                        self.indent_with(indentation_per_level);
                        self.write(&last_line[prefix_space_count..]);
                    }
                }
                InterpolPart::Interpolation(x) => {
                    if is_first {
                        self.indent_with(indentation_per_level);
                    }
                    let indentation_level = self.indentation_level;
                    self.format_node(x.syntax().clone());
                    self.indentation_level = indentation_level;
                }
            }

            is_first = false;
        }

        self.write("''");
        self.decrease_indentation();
    }

    /// Guesses the indentation type used in `expression`.
    fn guess_indentation_type(expression: &str) -> IndentationType {
        let indentations = expression
            .lines()
            .take(50)
            .map(|x| {
                let indentation = &x[..x.len() - x.trim_start().len()];
                if indentation.ends_with('\t') {
                    IndentationType::Tabs
                } else if indentation.len() % 4 == 0 && !indentation.is_empty() {
                    IndentationType::FourSpaces
                } else {
                    IndentationType::TwoSpaces
                }
            })
            .fold(HashMap::new(), |mut acc, x| {
                *acc.entry(x).or_insert(0) += 1;
                acc
            });
        let max = indentations
            .iter()
            .max_by_key(|x| (x.1, x.0))
            .map_or(IndentationType::TwoSpaces, |x| *x.0);
        if max == IndentationType::Tabs {
            IndentationType::Tabs
        } else if let Some(&x) = indentations.get(&IndentationType::FourSpaces)
            && x >= indentations.get(&IndentationType::TwoSpaces).unwrap_or(&0) * 3
        {
            IndentationType::FourSpaces
        } else {
            IndentationType::TwoSpaces
        }
    }

    /// Guesses the indentation level for `indentation` based on the indentation
    /// type of this formatter.
    fn guess_indentation_level(&self, indentation: &str) -> usize {
        let indentation_width: usize = indentation
            .chars()
            .map(|x| if x == '\t' { 4 } else { 1 })
            .sum();
        match self.indentation_type {
            IndentationType::TwoSpaces => indentation_width / 2,
            IndentationType::FourSpaces | IndentationType::Tabs => (indentation_width + 1) / 4,
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn child_formatter() {
        let mut parent = Formatter::new(IndentationType::TwoSpaces);
        parent.write("parent");
        parent.increase_indentation();
        let mut child = parent.child_formatter();
        child.open_line();
        child.write("child");
        assert_eq!(child.into_string(), "\n  child");
        assert_eq!(parent.into_string(), "parent");
    }

    #[test]
    fn line_separator_and_open_line() {
        fn test(indentation_type: IndentationType, expected: &str) {
            let mut formatter = Formatter::new(indentation_type);
            formatter.increase_indentation();
            formatter.increase_indentation();
            assert_eq!(formatter.line_separator(), expected, "{indentation_type:?}");
            formatter.open_line();
            assert_eq!(formatter.into_string(), expected, "{indentation_type:?}");
        }
        test(IndentationType::TwoSpaces, "\n    ");
        test(IndentationType::FourSpaces, "\n        ");
        test(IndentationType::Tabs, "\n\t\t");
    }

    #[test]
    fn increase_and_decrease_indentation() {
        let mut formatter = Formatter::new(IndentationType::TwoSpaces);
        formatter.increase_indentation();
        formatter.increase_indentation();
        assert_eq!(formatter.line_separator(), "\n    ");
        formatter.decrease_indentation();
        assert_eq!(formatter.line_separator(), "\n  ");
        formatter.decrease_indentation();
        formatter.decrease_indentation();
        assert_eq!(formatter.line_separator(), "\n");
    }

    #[test]
    fn set_and_reset_syntax_tree_indentation_offset() {
        let mut formatter = Formatter::new(IndentationType::TwoSpaces);
        formatter.increase_indentation();
        formatter.increase_indentation();
        formatter.set_syntax_tree_indentation_offset(4);
        assert_eq!(formatter.syntax_tree_indentation_offset, -2);
        formatter.reset_syntax_tree_indentation_offset();
        assert_eq!(formatter.syntax_tree_indentation_offset, 0);
        formatter.set_syntax_tree_indentation_offset(1);
        assert_eq!(formatter.syntax_tree_indentation_offset, 1);
    }

    #[test]
    fn format() {
        fn test(input: &str, expected: &str) {
            assert_eq!(Formatter::format(input).unwrap(), expected, "{input}");
        }
        test("let x =  1; in x", "let x =  1; in x");
        test(
            "{attr1 = {attr2 = true; attr3 = true;}; attr4 = true;}",
            "{attr1.attr2 = true; attr1.attr3 = true; attr4 = true;}",
        );
        test(
            "{\n  attr1.attr2 = true;\n  attr1.attr3 = [\n    \"\n x\"\n    ''\n  y\n''\n  ];\n\n  attr1.attr4 = true;\n  attr5 = true;\n}",
            "{\n  attr1 = {\n    attr2 = true;\n    attr3 = [\n      \"\n x\"\n      ''\n        y\n      ''\n    ];\n\n    attr4 = true;\n  };\n  attr5 = true;\n}",
        );
        test(
            "[\n  {\nattr1 = ''\nx\n'';}\n]",
            "[\n  {\n    attr1 = ''\n      x\n    '';\n  }\n]",
        );
        test("[\n   1\n  \n     2\n]", "[\n  1\n\n    2\n]");
        test(
            "{\n  a = let\n     x = ''x'';\n   in x;\n}",
            "{\n  a = let\n    x = ''x'';\n  in x;\n}",
        );
        test(
            "''  a\n  b\n   ${{attr1 = {attr2 = true;};}} \n ''",
            "''  a\n  b\n   ${{attr1.attr2 = true;}} \n''",
        );
        test("''\n${true}''", "''\n  ${true}''");
        test("\t\n\t\n''\na''", "\n\n''\n    a''");
        test("''a\n''", "''  a\n''");
        test("''a${true}a\n''", "''  a${true}a\n''");
        test("''${true\n    }\nbelow''", "''  ${true\n    }\n  below''");
    }

    #[test]
    #[should_panic]
    fn format_invalid() {
        Formatter::format("let").unwrap();
    }

    #[test]
    fn guess_indentation_type() {
        fn test(input: &str, expected: IndentationType) {
            let got = Formatter::guess_indentation_type(input);
            assert_eq!(got, expected, "{input}");
        }
        test("\tfirst\n    second\n\t\t\tthird", IndentationType::Tabs);
        test("    a\n        b\n    c\n  d", IndentationType::FourSpaces);
        test("\ta\n  b\n    c\n    d", IndentationType::TwoSpaces);
        test("{}", IndentationType::TwoSpaces);
    }

    #[test]
    fn guess_indentation_level() {
        let formatter = Formatter::new(IndentationType::TwoSpaces);
        assert_eq!(formatter.guess_indentation_level("  "), 1);
        assert_eq!(formatter.guess_indentation_level("   "), 1);
        assert_eq!(formatter.guess_indentation_level("\t"), 2);
        assert_eq!(formatter.guess_indentation_level(" \t"), 2);

        let formatter = Formatter::new(IndentationType::FourSpaces);
        assert_eq!(formatter.guess_indentation_level("  "), 0);
        assert_eq!(formatter.guess_indentation_level("    "), 1);
        assert_eq!(formatter.guess_indentation_level("   "), 1);
        assert_eq!(formatter.guess_indentation_level("\t "), 1);
        assert_eq!(formatter.guess_indentation_level("     "), 1);
        assert_eq!(formatter.guess_indentation_level("        "), 2);

        let formatter = Formatter::new(IndentationType::Tabs);
        assert_eq!(formatter.guess_indentation_level("\t"), 1);
        assert_eq!(formatter.guess_indentation_level("   "), 1);
        assert_eq!(formatter.guess_indentation_level(" \t "), 1);
        assert_eq!(formatter.guess_indentation_level("\t\t"), 2);
    }
}
