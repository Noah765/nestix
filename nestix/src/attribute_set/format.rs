use rnix::{SyntaxKind, ast::AttrSet};
use rowan::ast::AstNode;

use crate::parser::Parser;

/// A Nix attribute set format.
///
/// `AttributeSetFormat` supports two kinds of formats:
///
/// An inline format with an arbitrary amount of whitespace before and after the
/// braces, and a space between each element. For example:
/// ```nix
/// { a = true; /*Comment*/ b = true; }
/// ```
///
/// A multiline format, for example:
/// ```nix
/// {
///   a = true;
///   # Comment
///   b = true;
/// }
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AttributeSetFormat {
    Inline { surrounding_whitespace: String },
    Multiline,
}

impl AttributeSetFormat {
    /// Constructs a new `AttributeSetFormat` by looking at whitespace tokens under
    /// the given `node`.
    pub fn new(node: AttrSet) -> Self {
        let mut parser = Parser::new(node.syntax().clone());
        parser.skip_after(|x| x.kind() == SyntaxKind::TOKEN_L_BRACE);

        if parser.clone().contains_linebreaks() {
            Self::Multiline
        } else {
            Self::Inline {
                surrounding_whitespace: parser.next_whitespace(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rnix::{Root, ast::Expr};

    use super::*;

    fn parse_string_to_set(input: &str) -> AttrSet {
        match Root::parse(input).ok().unwrap().expr().unwrap() {
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
        test("rec\n{ }", " ");
        test("{a = true;}", "");
        test("/* test */ {  a = true;  }", "  ");
    }

    #[test]
    fn new_multiline() {
        fn test(set: &str) {
            assert_eq!(
                AttributeSetFormat::new(parse_string_to_set(set)),
                AttributeSetFormat::Multiline
            );
        }
        test("rec\n /* comment */\n {\n }");
        test("rec\n /* comment */\n {\n }");
        test("{\n \ta = true;}");
        test("{ a = true;\n \t}");
        test("{a = true;\n b = true;}");
    }
}
