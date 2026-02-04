use std::collections::HashMap;

use rnix::{
    SyntaxElement, SyntaxKind,
    ast::{AstToken, AttrSet, Whitespace},
};
use rowan::ast::AstNode;

/// A Nix attribute set format.
///
/// `AttributeSetFormat` supports two kinds of formats:
///
/// An inline format with an equal amount of whitespace before and after the
/// braces, and a space between each element. For example:
/// ```nix
/// { a = true; /*Comment*/ b = true; }
/// ```
///
/// A multiline format with an arbitrary amount of whitespace before each
/// element and per indentation level. For example:
/// ```nix
/// {
///   a = true;
///   # Comment
///   b = true;
/// }
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AttributeSetFormat {
    Inline {
        surrounding_whitespace: String,
    },
    Multiline {
        indentation: String,
        indentation_per_level: String,
    },
}

impl AttributeSetFormat {
    /// Constructs a new `AttributeSetFormat` by looking at whitespace tokens under
    /// the given `node`.
    pub fn new(node: AttrSet) -> Self {
        let mut line_breaking_whitespaces = node
            .syntax()
            .children_with_tokens()
            .skip_while(|x| x.kind() != SyntaxKind::TOKEN_L_BRACE)
            .filter_map(|x| SyntaxElement::into_token(x).and_then(Whitespace::cast))
            .filter(|x| x.syntax().text().contains('\n'))
            .peekable();

        if line_breaking_whitespaces.peek().is_none() {
            return Self::Inline {
                surrounding_whitespace: node
                    .syntax()
                    .children_with_tokens()
                    .skip_while(|x| x.kind() != SyntaxKind::TOKEN_L_BRACE)
                    .nth(1)
                    .filter(|x| x.kind() == SyntaxKind::TOKEN_WHITESPACE)
                    .map_or_else(String::new, |x| x.to_string()),
            };
        }

        let mut indentations = HashMap::new();
        let mut last_line_indentation = None;
        while let Some(x) = line_breaking_whitespaces.next() {
            let indentation = x
                .syntax()
                .text()
                .lines()
                .last()
                .map(|x| String::from(&x[..x.len() - x.trim_start().len()]))
                .expect("`str::lines()` should not be empty");

            if line_breaking_whitespaces.peek().is_none() {
                last_line_indentation = Some(indentation);
            } else {
                *indentations.entry(indentation).or_insert(0) += 1;
            }
        }
        let last_line_indentation = last_line_indentation
            .expect("`last_line_indentation` should contain the last indentation of the non-empty `line_breaking_whitespaces`");

        if indentations.is_empty() {
            let indentation_per_level = if last_line_indentation.ends_with('\t') {
                "\t"
            } else {
                "  "
            };
            Self::Multiline {
                indentation: format!("{last_line_indentation}{indentation_per_level}"),
                indentation_per_level: String::from(indentation_per_level),
            }
        } else {
            let indentation = indentations
                .into_iter()
                .max_by_key(|x| x.1)
                .expect("`indentations` should not be empty")
                .0;
            let indentation_per_level =
                if indentation.strip_prefix(&last_line_indentation) == Some("    ") {
                    "    "
                } else if indentation.ends_with('\t') {
                    "\t"
                } else {
                    "  "
                };
            Self::Multiline {
                indentation,
                indentation_per_level: String::from(indentation_per_level),
            }
        }
    }

    /// Creates a new `AttributeSetFormat` corresponding to the expected format
    /// of an attribute set which is a value of an attribute set in this
    /// format.
    pub fn subset_format(&self) -> Self {
        match self {
            Self::Inline { .. } => self.clone(),
            Self::Multiline {
                indentation,
                indentation_per_level,
            } => Self::Multiline {
                indentation: indentation.clone() + indentation_per_level,
                indentation_per_level: indentation_per_level.clone(),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rnix::{Root, ast::Expr};

    use super::*;

    fn parse_string_to_set(input: &str) -> AttrSet {
        match Root::parse(input).tree().expr().unwrap() {
            Expr::AttrSet(x) => x,
            _ => panic!(),
        }
    }

    #[test]
    fn new_inline() {
        fn test(set: &str, whitespace: &str) {
            assert_eq!(
                AttributeSetFormat::new(parse_string_to_set(set)),
                AttributeSetFormat::Inline {
                    surrounding_whitespace: String::from(whitespace)
                }
            );
        }
        test("rec {}", "");
        test("rec /* comment */  {\t}", "\t");
        test("{a = true;}", "");
        test("/* test */ {  a = true;  }", "  ");
    }

    #[test]
    fn new_multiline() {
        fn test(set: &str, indentation: &str, indentation_per_level: &str) {
            assert_eq!(
                AttributeSetFormat::new(parse_string_to_set(set)),
                AttributeSetFormat::Multiline {
                    indentation: String::from(indentation),
                    indentation_per_level: String::from(indentation_per_level)
                }
            );
        }
        test("rec\n /* comment */\n {\n }", "   ", "  ");
        test("{ a = true;\n \t}", " \t\t", "\t");
        test("{\n \ta = true;}", " \t\t", "\t");
        test("rec {\na = true;\n b = true;\n c = true;\n}", " ", "  ");
        test("{\n    a = true;\n/* comment */ }", "    ", "    ");
        test("rec {\n   a = true;\n\t}", "   ", "  ");
        test("rec {\n \ta = true;\n }", " \t", "\t");
        test("{\n a = true;\n b = true;\n\t}", " ", "  ");
    }

    #[test]
    fn subset_format() {
        assert_eq!(
            AttributeSetFormat::Inline {
                surrounding_whitespace: String::from(" ")
            }
            .subset_format(),
            AttributeSetFormat::Inline {
                surrounding_whitespace: String::from(" ")
            }
        );
        assert_eq!(
            AttributeSetFormat::Multiline {
                indentation: String::from(" \t \t"),
                indentation_per_level: String::from("\t")
            }
            .subset_format(),
            AttributeSetFormat::Multiline {
                indentation: String::from(" \t \t\t"),
                indentation_per_level: String::from("\t")
            }
        );
    }
}
