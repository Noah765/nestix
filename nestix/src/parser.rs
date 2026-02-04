use std::iter::Peekable;

use rnix::{
    SyntaxElement, SyntaxElementChildren, SyntaxNode, SyntaxToken,
    ast::{self, AstToken, Attr, Attrpath, Expr, Whitespace},
};
use rowan::ast::AstNode;

use crate::comment::Comment;

/// A Nix `SyntaxElement` parser.
///
/// Parses child elements of a given `SyntaxNode` into comments, attributes, etc.
#[derive(Clone)]
pub struct Parser(Peekable<SyntaxElementChildren>);

impl Parser {
    /// Constructs a new `Parser`, which parses the child elements of `parent`.
    pub fn new(parent: SyntaxNode) -> Self {
        Self(parent.children_with_tokens().peekable())
    }

    /// Skips over the next element if it is a whitespace token.
    pub fn skip_whitespace(&mut self) {
        if let Some(x) = self.0.peek()
            && Whitespace::can_cast(x.kind())
        {
            self.0.next();
        }
    }

    /// Skips over elements until `predicate` returns true, also skipping that
    /// element.
    pub fn skip_after(&mut self, predicate: impl FnMut(&SyntaxElement) -> bool) {
        self.0.find(predicate);
    }

    /// Returns the next element without consuming it.
    pub fn peek(&mut self) -> Option<SyntaxElement> {
        self.0.peek().cloned()
    }

    /// Consumes and returns the next element.
    pub fn next(&mut self) -> Option<SyntaxElement> {
        self.0.next()
    }

    /// Consumes the following element if it is a whitespace token, then returns
    /// its text or an empty string.
    pub fn next_whitespace(&mut self) -> String {
        if let Some(x) = self.0.peek().and_then(SyntaxElement::as_token)
            && Whitespace::can_cast(x.kind())
        {
            let text = String::from(x.text());
            self.0.next();
            text
        } else {
            String::new()
        }
    }

    /// Consumes elements until the end or a non-trivia element is reached, then
    /// returns the encountered comments.
    pub fn next_comments(&mut self) -> Vec<Comment> {
        self.next_comments_while(|_| true)
    }

    /// Consumes elements until the end, an empty line or a non-trivia element
    /// is reached, then returns the encountered comments.
    pub fn next_comment_section(&mut self) -> Vec<Comment> {
        self.next_comments_while(|x| x.matches('\n').nth(1).is_none())
    }

    /// Consumes elements until the end, a linebreak or a non-trivia element is
    /// reached, then returns the encountered comments.
    pub fn next_comment_line(&mut self) -> Vec<Comment> {
        self.next_comments_while(|x| !x.contains('\n'))
    }

    #[inline]
    fn next_comments_while(
        &mut self,
        mut whitespace_predicate: impl FnMut(&str) -> bool,
    ) -> Vec<Comment> {
        let mut comments = Vec::new();

        while let Some(x) = self.0.peek().and_then(SyntaxElement::as_token) {
            if ast::Comment::can_cast(x.kind()) {
                comments.push(Comment::new(x.text()));
                self.0.next();
            } else if Whitespace::can_cast(x.kind()) && whitespace_predicate(x.text()) {
                self.0.next();
            } else {
                break;
            }
        }

        comments
    }

    /// Consumes and returns the following token.
    ///
    /// # Panics
    ///
    /// Panics if `Parser` is at the end or if the next element is not a token.
    pub fn next_token(&mut self) -> SyntaxToken {
        self.0
            .next()
            .and_then(SyntaxElement::into_token)
            .expect("the next element should be a token")
    }

    /// Consumes and returns the following node.
    ///
    /// # Panics
    ///
    /// Panics if `Parser` is at the end or if the next element is not a node.
    pub fn next_node(&mut self) -> SyntaxNode {
        self.0
            .next()
            .and_then(SyntaxElement::into_node)
            .expect("the next element should be a node")
    }

    /// Consumes and returns the following expression node.
    ///
    /// # Panics
    ///
    /// Panics if `Parser` is at the end or if the next element is not an
    /// expression node.
    pub fn next_expression(&mut self) -> Expr {
        Expr::cast(self.next_node()).expect("the next element should be an expression node")
    }

    /// Consumes and returns the following attribute path node.
    ///
    /// # Panics
    ///
    /// Panics if `Parser` is at the end or if the next element is not an attribute
    /// path node.
    pub fn next_attribute_path(&mut self) -> Attrpath {
        Attrpath::cast(self.next_node()).expect("the next element should be an attribute path node")
    }

    /// Consumes and returns the following attribute node.
    ///
    /// # Panics
    ///
    /// Panics if `Parser` is at the end or if the next element is not an attribute
    /// node.
    pub fn next_attribute(&mut self) -> Attr {
        Attr::cast(self.next_node()).expect("the next element should be an attribute node")
    }

    /// Returns `true` if a whitespace token with a newline character is left.
    pub fn contains_linebreaks(self) -> bool {
        self.0
            .filter_map(|x| SyntaxElement::into_token(x).and_then(Whitespace::cast))
            .any(|x| x.syntax().text().contains('\n'))
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rnix::{
        Root,
        ast::{Expr, HasEntry, Literal},
    };

    use super::*;

    #[test]
    fn skip_whitespace() {
        let mut parser = Parser::new(Root::parse(" \t\n\n 0").syntax());
        parser.skip_whitespace();
        assert!(Literal::can_cast(parser.0.next().unwrap().kind()));
    }

    #[test]
    fn skip_after() {
        let mut parser = Parser::new(Root::parse(" \t\n/* Comment */ 0/* After */").syntax());
        parser.skip_after(|x| Literal::can_cast(x.kind()));
        let next_token = parser.0.next().unwrap().into_token().unwrap();
        assert_eq!(next_token.text(), "/* After */");
    }

    #[test]
    fn peek() {
        let mut parser = Parser::new(Root::parse("0").syntax());
        assert_eq!(parser.peek().unwrap().to_string(), "0");
        assert!(parser.peek().is_some());
    }

    #[test]
    fn next() {
        let mut parser = Parser::new(Root::parse("0").syntax());
        assert_eq!(parser.next().unwrap().to_string(), "0");
        assert!(parser.next().is_none());
    }

    #[test]
    fn next_whitespace() {
        let mut parser = Parser::new(Root::parse(" \t\n\n 0").syntax());
        assert_eq!(parser.next_whitespace(), " \t\n\n ");
    }

    #[test]
    fn next_comments() {
        fn test(code: &str, comments: &[&str], comment_section: &[&str], comment_line: &[&str]) {
            for x in [comments, comment_section, comment_line].into_iter().zip([
                Parser::next_comments,
                Parser::next_comment_section,
                Parser::next_comment_line,
            ]) {
                assert_eq!(
                    x.1(&mut Parser::new(Root::parse(code).syntax())),
                    x.0.into_iter().map(|x| Comment::new(x)).collect::<Vec<_>>()
                );
            }
        }
        test(" 0", &[], &[], &[]);
        test(
            "# Comment \n0",
            &["# Comment "],
            &["# Comment "],
            &["# Comment "],
        );
        test(
            "/* Comment */  0",
            &["/* Comment */"],
            &["/* Comment */"],
            &["/* Comment */"],
        );
        test(
            "/* Comment\n\n\n*/ /* second */ \n#third\n\n# fourth\n0",
            &["/* Comment\n\n\n*/", "/* second */", "#third", "# fourth"],
            &["/* Comment\n\n\n*/", "/* second */", "#third"],
            &["/* Comment\n\n\n*/", "/* second */"],
        );
    }

    #[test]
    fn next_token() {
        assert_eq!(
            Parser::new(Root::parse("/**/0").syntax().clone())
                .next_token()
                .to_string(),
            String::from("/**/")
        );
    }

    #[test]
    #[should_panic]
    fn next_token_invalid() {
        Parser::new(Root::parse("0").syntax()).next_token();
    }

    #[test]
    fn next_node() {
        assert_eq!(
            Parser::new(Root::parse("0").syntax().clone())
                .next_node()
                .to_string(),
            String::from("0")
        );
    }

    #[test]
    #[should_panic]
    fn next_node_invalid() {
        Parser::new(Root::parse(" 0").syntax()).next_node();
    }

    #[test]
    fn next_expression() {
        assert_eq!(
            Parser::new(Root::parse("0").syntax().clone())
                .next_expression()
                .to_string(),
            String::from("0")
        );
    }

    #[test]
    #[should_panic]
    fn next_expression_invalid() {
        Parser::new(Root::parse(" 0").syntax()).next_expression();
    }

    #[test]
    fn next_attribute_path() {
        let attr_path_parent = match Root::parse("{attr1 = true;}").ok().unwrap().expr().unwrap() {
            Expr::AttrSet(x) => x.attrpath_values().next().unwrap(),
            _ => panic!(),
        };
        let path = Parser::new(attr_path_parent.syntax().clone())
            .next_attribute_path()
            .to_string();
        assert_eq!(path, String::from("attr1"));
    }

    #[test]
    #[should_panic]
    fn next_attribute_path_invalid() {
        Parser::new(Root::parse("0").syntax()).next_attribute_path();
    }

    #[test]
    fn next_attribute() {
        let attr_parent = match Root::parse("{attr1 = true;}").ok().unwrap().expr().unwrap() {
            Expr::AttrSet(x) => x.attrpath_values().next().unwrap().attrpath().unwrap(),
            _ => panic!(),
        };
        let name = Parser::new(attr_parent.syntax().clone())
            .next_attribute()
            .to_string();
        assert_eq!(name, String::from("attr1"));
    }

    #[test]
    #[should_panic]
    fn next_attribute_invalid() {
        Parser::new(Root::parse("0").syntax()).next_attribute();
    }

    #[test]
    fn contains_linebreaks() {
        fn test(code: &str, expected: bool) {
            assert_eq!(
                Parser::new(Root::parse(code).syntax()).contains_linebreaks(),
                expected
            );
        }
        test("\n0", true);
        test("10 + 30 \n", true);
        test("10 + 30", false);
        test("10 +\n 30", false);
        test("{\n}", false);
        test("", false);
    }
}
