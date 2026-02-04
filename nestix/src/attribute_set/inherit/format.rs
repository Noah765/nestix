use rnix::{
    SyntaxElement, SyntaxKind,
    ast::{AstToken, Inherit, Whitespace},
};
use rowan::ast::AstNode;

/// A Nix inherit formatting style.
///
/// `InheritFormat` supports two kinds of formats:
///
/// An inline format, for example:
/// ```nix
/// {
///   inherit (from) attr1 attr2 attr3;
/// }
/// ```
///
/// A multiline format with the from segment optionally on a separate line,
/// for example:
/// ```nix
/// {
///   inherit
///     (from)
///     attr1
///     attr2
///     attr3
///     ;
/// }
/// ```
#[derive(Debug, PartialEq, Eq)]
pub enum InheritFormat {
    Inline,
    Multiline { from_on_separate_line: bool },
}

impl InheritFormat {
    /// Constructs a new `InheritFormat` based on the whitespace tokens under
    /// the given `node`.
    pub fn new(node: Inherit) -> Self {
        if !node
            .syntax()
            .children_with_tokens()
            .filter_map(|x| SyntaxElement::into_token(x).and_then(Whitespace::cast))
            .any(|x| x.syntax().text().contains('\n'))
        {
            Self::Inline
        } else {
            Self::Multiline {
                from_on_separate_line: node
                    .syntax()
                    .children_with_tokens()
                    .take_while(|x| x.kind() != SyntaxKind::NODE_INHERIT_FROM)
                    .filter_map(|x| SyntaxElement::into_token(x).and_then(Whitespace::cast))
                    .any(|x| x.syntax().text().contains('\n')),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rnix::{
        Root,
        ast::{Expr, HasEntry},
    };

    use super::*;

    fn parse_string_to_inherit(set: &str) -> Inherit {
        match Root::parse(set).tree().expr().unwrap() {
            Expr::AttrSet(x) => x.inherits().next().unwrap(),
            _ => panic!(),
        }
    }

    #[test]
    fn new_inline() {
        fn test(set: &str) {
            let inherit = parse_string_to_inherit(set);
            assert_eq!(InheritFormat::new(inherit), InheritFormat::Inline);
        }
        test("{inherit  (from) attr1\tattr2;}");
        test("{\n  inherit (from)  attr1 attr2\t;\n}");
    }

    #[test]
    fn new_multiline() {
        fn test(set: &str, from_on_separate_line: bool) {
            assert_eq!(
                InheritFormat::new(parse_string_to_inherit(set)),
                InheritFormat::Multiline {
                    from_on_separate_line: from_on_separate_line,
                }
            );
        }
        test("{inherit\n(from) attr1;}", true);
        test("{inherit (from) attr1\nattr2;}", false);
        test("{\n  inherit /* comment */ \t(from) \n   attr1;\n}", false);
        test("{\n  inherit #t\n/* comment */ \t(from) attr1;\n}", true);
    }
}
