use std::{iter::Peekable, ops::Range};

use super::ast::*;
use ariadne::{Label, Report, ReportBuilder, ReportKind};
#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxError {
    UnexpectedToken { expected: String, got: Token },
    InvalidLiteral(Token),
    InvalidEscSeq(Spanned<char>),
    UnexpectedPattern(Spanned<Pattern>, String),
    UnexpectedEof(Token),
    InvalidAssignment(SpanExpr),
    EmptyImport(Span),
}

pub type ParseResult<T> = Result<T, SyntaxError>;

impl SyntaxError {
    pub fn report(&self, filename: String, file: &str) -> ReportBuilder<(String, Range<usize>)> {
        match self {
            SyntaxError::EmptyImport(span) => {
                Report::build(ReportKind::Error, &filename, span.start)
                    .with_message("empty import")
                    .with_label(Label::new((filename.clone(), (*span).into())))
            }
            SyntaxError::UnexpectedToken { expected, got } => {
                Report::build(ReportKind::Error, &filename, got.span.start)
                    .with_message("Unexpected token")
                    .with_label(
                        Label::new((filename.clone(), got.span.into()))
                            .with_message(format!("Expected {expected}, got {got}")),
                    )
            }
            SyntaxError::UnexpectedPattern(x, expected) => {
                Report::build(ReportKind::Error, &filename, x.span.start)
                    .with_message("Unexpected pattern")
                    .with_label(
                        Label::new((filename.clone(), x.span.into()))
                            .with_message(format!("Expected {expected}, got {x}")),
                    )
            }
            SyntaxError::InvalidAssignment(expr) => {
                Report::build(ReportKind::Error, &filename, expr.span.start)
                    .with_message("Invalid assignment")
                    .with_label(
                        Label::new((filename.clone(), expr.span.into()))
                            .with_message(format!("RValue expected, got {expr}")),
                    )
            }
            SyntaxError::InvalidLiteral(t) => {
                Report::build(ReportKind::Error, &filename, t.span.start)
                    .with_message("Invalid literal")
                    .with_label(
                        Label::new((filename.clone(), t.span.into()))
                            .with_message(format!("Invalid literal '{}'", t.text(file))),
                    )
            }
            SyntaxError::InvalidEscSeq(c) => {
                Report::build(ReportKind::Error, &filename, c.span.start)
                    .with_message("Invalid escape sequence")
                    .with_label(
                        Label::new((filename.clone(), c.span.into()))
                            .with_message(format!("Invalid escape sequence '\\{}'", c.node)),
                    )
            }
            SyntaxError::UnexpectedEof(t) => {
                Report::build(ReportKind::Error, &filename, t.span.start)
                    .with_message("Unexpected EOF")
                    .with_label(
                        Label::new((filename.clone(), t.span.into()))
                            .with_message("Unexpected EOF"),
                    )
            }
        }
    }
}
macro_rules! spanned {
    ($span:expr, $node:expr) => {
        $crate::ast::Spanned {
            span: $span.into(),
            node: $node,
        }
    };
}

#[allow(dead_code)]
/// The parser and all associated state
pub struct Parser<'input> {
    pub input: &'input str,
    lexer: Peekable<Lexer<'input>>,
}

impl<'input> Parser<'input> {
    /// Create a new parser that operates on the given `input`
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            lexer: Lexer::new(input).peekable(),
        }
    }

    /// Return the next token, or if there are no more tokens: `SyntaxError::UnexpectedEof`
    fn next(&mut self) -> ParseResult<Token> {
        self.lexer.next().ok_or_else(|| {
            let len = self.input.len();
            SyntaxError::UnexpectedEof(Token {
                kind: TokenKind::Eof,
                span: (len..len).into(),
            })
        })
    }

    /// Peek the `kind` of the next token without consuming
    fn peek(&mut self) -> TokenKind {
        self.lexer
            .peek()
            .map(|token| token.kind)
            .unwrap_or(TokenKind::Eof)
    }

    fn peek_text(&mut self) -> Option<&str> {
        self.lexer.peek().copied().map(|token| self.text(token))
    }
    /// Consume the next token and check that its kind is as `expected`, returning
    /// `SyntaxError::UnexpectedToken` if not
    fn consume(&mut self, expected: TokenKind) -> ParseResult<()> {
        let token = self.next()?;
        if token.kind != expected {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_string(),
                got: token,
            })
        } else {
            Ok(())
        }
    }

    /// Checks that the next token's kind is as `expected` and also returns the token
    fn consume_next(&mut self, expected: TokenKind) -> ParseResult<Token> {
        let token = self.next()?;
        if token.kind != expected {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_string(),
                got: token,
            })
        } else {
            Ok(token)
        }
    }

    /// Advance without checking anything
    fn advance(&mut self) {
        self.lexer.next().unwrap();
    }

    /// Peek the next token and check if its kind is `kind`
    fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    /// Peek the next token and check if its kind is one of `kinds`
    #[allow(dead_code)]
    fn at_any<const N: usize>(&mut self, kinds: [TokenKind; N]) -> bool {
        kinds.contains(&self.peek())
    }

    /// Obtain source text behind given token
    fn text(&self, token: Token) -> &'input str {
        token.text(self.input)
    }
}

pub type SpanExpr = Spanned<Expr>;

impl Parser<'_> {
    pub fn expr(&mut self) -> ParseResult<SpanExpr> {
        match self.peek() {
            TokenKind::BraceOpen => {
                let t = self.next()?;
                match self.peek() {
                    /*TokenKind::Ident => {
                        let ident = self.ident()?;

                        if let TokenKind::Colon = self.peek() {
                            let mut fields = vec![];
                            self.advance();
                            let expr = self.expr()?;
                            let mut last = expr.span;
                            fields.push((ident.node, expr));
                            if let TokenKind::Comma = self.peek() {
                                self.advance();
                            }
                            fields.extend_from_slice(&self.parse_list(
                                TokenKind::Comma,
                                TokenKind::BraceClose,
                                |p| {
                                    let id = p.ident()?;
                                    p.consume(TokenKind::Colon)?;
                                    let e = p.expr()?;
                                    last = e.span;
                                    Ok((id.node, e))
                                },
                            )?);

                            return Ok(spanned!(t.span.start..last.end, Expr::Object(fields)));
                        } else {
                            let e =
                                self.expr_next(spanned!(ident.span, Expr::Ident(ident.node)))?;
                            let b = self.parse_block(t.span)?;
                            let sp = b.span;
                            match b.node {
                                Expr::Block(x) => {
                                    let mut b = vec![e];
                                    b.extend_from_slice(&x);
                                    Ok(spanned!(t.span.start..sp.end, Expr::Block(b)))
                                }
                                _ => unreachable!(),
                            }
                        }
                    }*/
                    _ => self.parse_block(t.span),
                }
            }
            TokenKind::If => {
                let t = self.next()?;
                let cond = self.expr()?;
                self.consume(TokenKind::Then)?;
                let then = self.expr()?;
                let otherwise = if let TokenKind::Else = self.peek() {
                    self.advance();
                    Some(Box::new(self.expr()?))
                } else {
                    None
                };

                Ok(spanned!(
                    t.span.start
                        ..otherwise
                            .as_ref()
                            .map(|x| x.span.end)
                            .unwrap_or(then.span.end),
                    Expr::If(Box::new(cond), Box::new(then), otherwise)
                ))
            }
            TokenKind::Try => {
                let t = self.next()?;
                let try_ = self.expr()?;
                self.consume(TokenKind::Catch)?;
                let id = self.ident()?;
                let catch = self.expr()?;
                Ok(spanned!(
                    t.span.start..catch.span.end,
                    Expr::Try(Box::new(try_), id.node, Box::new(catch))
                ))
            }
            TokenKind::While => {
                let t = self.next()?;
                let cond = self.expr()?;
                let body = self.expr()?;
                Ok(spanned!(
                    t.span.start..body.span.end,
                    Expr::While(Box::new(cond), Box::new(body), false)
                ))
            }
            TokenKind::Do => {
                let t = self.next()?;
                let body = self.expr()?;
                self.consume(TokenKind::While)?;
                let cond = self.expr()?;
                Ok(spanned!(
                    t.span.start..cond.span.end,
                    Expr::While(Box::new(cond), Box::new(body), true)
                ))
            }
            TokenKind::Let => self.parse_let(),
            TokenKind::Function => self.parse_function(),
            TokenKind::ParenOpen => {
                let t = self.next()?;
                let e = self.expr()?;
                self.consume(TokenKind::ParenClose)?;
                Ok(spanned!(
                    t.span.start..e.span.end,
                    Expr::Parenthesis(Box::new(e))
                ))
            }
            TokenKind::Import => {
                let t = self.next()?;
                if let TokenKind::Str = self.peek() {
                    let str = self.next()?;
                    let path = self.text(str);
                    Ok(spanned!(
                        t.span.start..str.span.end,
                        Expr::ImportStr(path.to_string())
                    ))
                } else {
                    let modules =
                        self.parse_list(TokenKind::Dot, TokenKind::Semicolon, Self::ident)?;
                    if modules.is_empty() {
                        return Err(SyntaxError::EmptyImport(t.span));
                    }
                    let span = modules.last().unwrap().span;
                    Ok(spanned!(
                        t.span.start..span.end,
                        Expr::Import(modules.iter().map(|x| x.node.clone()).collect())
                    ))
                }
            }
            TokenKind::Export => {
                let t = self.next()?;
                self.consume(TokenKind::BraceOpen)?;
                let exports = self.parse_list(TokenKind::Comma, TokenKind::BraceClose, |p| {
                    let id = p.ident()?;
                    if let TokenKind::Ident = p.peek() {
                        let x = p.next()?;
                        let t = p.text(x);
                        if t == "as" {
                            let next = p.ident()?;

                            Ok(Export::Alias(id.node, next.node))
                        } else {
                            Err(SyntaxError::UnexpectedToken {
                                expected: "comma or 'as'".to_string(),
                                got: x,
                            })
                        }
                    } else {
                        Ok(Export::Identifier(id.node))
                    }
                })?;

                Ok(spanned!(t.span, Expr::Export(exports)))
            }
            _ => {
                if let Some(e) = self.expr_short()? {
                    Ok(e)
                } else {
                    let t = self.next()?;
                    Err(SyntaxError::UnexpectedToken {
                        got: t,
                        expected: "expression".to_string(),
                    })
                }
            }
        }
    }
    /// Parse string literal, including escape sequences
    fn parse_string(token: Token, text: &str) -> ParseResult<String> {
        let mut buf = vec![];
        let mut backslash = false;
        for (i, byte) in text[1..(text.len() - 1)].bytes().enumerate() {
            if backslash {
                match byte {
                    b't' => buf.push(b'\t'),
                    b'n' => buf.push(b'\n'),
                    b @ b'"' | b @ b'\\' => buf.push(b),
                    b => {
                        return Err(SyntaxError::InvalidEscSeq(spanned!(
                            (i..(i + 2)),
                            char::from(b)
                        )))
                    }
                };
                backslash = false;
            } else {
                if byte == b'\\' {
                    backslash = true;
                } else {
                    buf.push(byte);
                }
            }
        }

        String::from_utf8(buf).map_err(|_| SyntaxError::InvalidLiteral(token))
    }

    pub fn ident(&mut self) -> ParseResult<Spanned<String>> {
        let tok = self.consume_next(TokenKind::Ident)?;
        Ok(spanned!(tok.span, self.text(tok).to_string()))
    }

    pub fn parse_switch(&mut self) -> ParseResult<SpanExpr> {
        let l = self.consume_next(TokenKind::Switch)?;
        let cond = self.expr()?;
        self.consume(TokenKind::BraceOpen)?;
        let mut last = cond.span;
        let cases = self.parse_list(TokenKind::BitOr, TokenKind::BraceClose, |parser| {
            let pat = parser.parse_pattern()?;
            let when = if let TokenKind::If = parser.peek() {
                parser.advance();
                Some(parser.expr()?)
            } else {
                None
            };
            let then = parser.expr()?;
            last = then.span;
            Ok((pat, when, then))
        })?;

        Ok(spanned!(
            l.span.start..last.end,
            Expr::Switch(Box::new(cond), cases, None)
        ))
    }

    pub fn parse_let(&mut self) -> ParseResult<SpanExpr> {
        let l = self.consume_next(TokenKind::Let)?;
        let rec = if let TokenKind::Rec = self.peek() {
            self.consume(TokenKind::Rec)?;
            true
        } else {
            false
        };
        let mut defs = vec![];
        let mut last;
        let body = loop {
            let name = self.parse_destructive_assignment()?;
            match self.peek() {
                TokenKind::ParenOpen => {
                    self.advance();
                    self.consume(TokenKind::ParenClose)?;
                    self.consume(TokenKind::Assign)?;
                    let body = self.expr()?;
                    last = body.span;
                    defs.push(spanned!(
                        name.span.start..body.span.end,
                        LetDef::Function(
                            match name.node {
                                Pattern::Ident(x) => x,
                                _ =>
                                    return Err(SyntaxError::UnexpectedPattern(
                                        name,
                                        "identifier".to_string(),
                                    )),
                            },
                            vec![],
                            Box::new(body)
                        )
                    ));
                }
                TokenKind::Assign => {
                    self.advance();
                    let expr = self.expr()?;
                    last = expr.span;
                    defs.push(spanned!(
                        name.span.start..expr.span.end,
                        LetDef::Variable(name, Box::new(expr))
                    ));
                }
                TokenKind::Ident => {
                    let params = self.fn_params(TokenKind::Assign)?;
                    let body = self.expr()?;
                    last = body.span;
                    defs.push(spanned!(
                        name.span.start..body.span.end,
                        LetDef::Function(
                            match name.node {
                                Pattern::Ident(x) => x,
                                _ =>
                                    return Err(SyntaxError::UnexpectedPattern(
                                        name,
                                        "identifier".to_string(),
                                    )),
                            },
                            params,
                            Box::new(body)
                        )
                    ));
                }
                _ => {
                    return Err(SyntaxError::UnexpectedToken {
                        expected: format!("variable or function definition"),
                        got: self.next()?,
                    })
                }
            }

            match self.peek() {
                TokenKind::And => {
                    self.advance();
                    continue;
                }
                TokenKind::In => {
                    self.advance();
                    let e = self.expr()?;
                    last = e.span;
                    break Some(e);
                }
                _ => break None,
            }
        };

        Ok(spanned!(
            l.span.start..last.end,
            Expr::Let(defs, body.map(|x| Box::new(x)), rec)
        ))
    }

    pub fn parse_destructive_assignment(&mut self) -> ParseResult<Spanned<Pattern>> {
        let tok = self.next()?;
        match tok.kind {
            TokenKind::Ident => Ok(spanned!(
                tok.span,
                Pattern::Ident(self.text(tok).to_string())
            )),

            TokenKind::BracketOpen => {
                let patterns = self.parse_list(
                    TokenKind::Comma,
                    TokenKind::BracketClose,
                    Self::parse_pattern,
                );

                Ok(spanned!(tok.span, Pattern::Array(patterns?)))
            }
            TokenKind::BraceOpen => {
                let p = self.parse_list(TokenKind::Comma, TokenKind::BraceClose, |parser| {
                    let name = parser.ident()?;
                    match parser.peek() {
                        TokenKind::Comma | TokenKind::BraceClose => return Ok((name.node, None)),
                        TokenKind::Colon => {
                            let id = parser.ident()?;
                            return Ok((
                                name.node,
                                Some(spanned!(id.span, Pattern::Ident(id.node))),
                            ));
                        }
                        _ => {
                            let tok = parser.next()?;
                            Err(SyntaxError::UnexpectedToken {
                                expected: format!("field name"),
                                got: tok,
                            })
                        }
                    }
                })?;

                Ok(spanned!(tok.span, Pattern::Record(p)))
            }
            _ => {
                return Err(SyntaxError::UnexpectedToken {
                    expected: format!("destructuring pattern or identifier"),
                    got: tok,
                })
            }
        }
    }

    pub fn parse_pattern_decl(&mut self) -> ParseResult<Spanned<Pattern>> {
        let tok = self.next()?;
        match tok.kind {
            TokenKind::Int => Ok(spanned!(
                tok.span,
                Pattern::Int(
                    self.text(tok)
                        .parse()
                        .map_err(|_| { SyntaxError::InvalidLiteral(tok) })?
                )
            )),
            TokenKind::Float => Ok(spanned!(
                tok.span,
                Pattern::Float(
                    self.text(tok)
                        .parse()
                        .map_err(|_| { SyntaxError::InvalidLiteral(tok) })?
                )
            )),
            TokenKind::Str => Ok(spanned!(tok.span, Pattern::Str(self.text(tok).to_string()))),
            TokenKind::Ident => Ok(spanned!(
                tok.span,
                Pattern::Ident(self.text(tok).to_string())
            )),
            TokenKind::True => Ok(spanned!(tok.span, Pattern::Bool(true))),
            TokenKind::False => Ok(spanned!(tok.span, Pattern::Bool(false))),
            TokenKind::Colon => {
                let id = self.ident()?;
                Ok(spanned!(
                    tok.span.start..id.span.end,
                    Pattern::Symbol(id.node)
                ))
            }
            TokenKind::BracketOpen => {
                let patterns = self.parse_list(
                    TokenKind::Comma,
                    TokenKind::BracketClose,
                    Self::parse_pattern,
                );

                Ok(spanned!(tok.span, Pattern::Array(patterns?)))
            }
            TokenKind::BraceOpen => {
                let p = self.parse_list(TokenKind::Comma, TokenKind::BraceClose, |parser| {
                    let name = parser.ident()?;
                    match parser.peek() {
                        TokenKind::Comma => return Ok((name.node, None)),
                        TokenKind::Colon => {
                            parser.advance();
                            let p = parser.parse_pattern()?;
                            Ok((name.node, Some(p)))
                        }
                        _ => {
                            let tok = parser.next()?;
                            Err(SyntaxError::UnexpectedToken {
                                expected: format!("record pattern"),
                                got: tok,
                            })
                        }
                    }
                })?;

                Ok(spanned!(tok.span, Pattern::Record(p)))
            }
            _ => {
                return Err(SyntaxError::UnexpectedToken {
                    expected: format!("pattern"),
                    got: tok,
                })
            }
        }
    }

    fn parse_pattern(&mut self) -> ParseResult<Spanned<Pattern>> {
        let p = self.parse_pattern_decl()?;
        match self.peek() {
            TokenKind::Ident if self.peek_text() == Some("as") => {
                self.advance();
                let id = self.ident()?;
                Ok(spanned!(
                    p.span.start..id.span.end,
                    Pattern::Alias(id.node, Box::new(p))
                ))
            }
            _ => Ok(p),
        }
    }

    pub fn parse_function(&mut self) -> ParseResult<SpanExpr> {
        let f = self.consume_next(TokenKind::Function)?;

        let params = self.fn_params(TokenKind::Arrow)?;
        let body = self.expr()?;
        Ok(spanned!(
            f.span.start..body.span.end,
            Expr::Function(None, params, Box::new(body))
        ))
    }

    // a b c ->
    pub fn fn_params(&mut self, ends_with: TokenKind) -> ParseResult<Vec<String>> {
        let mut params = vec![];
        loop {
            let tok = self.next()?;
            match tok.kind {
                TokenKind::Ident => {
                    let id = self.text(tok).to_string();
                    params.push(id);
                }
                x if x == ends_with => break,
                _ => {
                    return Err(SyntaxError::UnexpectedToken {
                        expected: format!("expected '{}' or argument name", ends_with),
                        got: tok,
                    })
                }
            }
        }
        Ok(params)
    }

    pub fn expr_next(&mut self, e: SpanExpr) -> ParseResult<SpanExpr> {
        match self.peek() {
            TokenKind::ParenOpen => {
                self.advance();
                let (span, pl) = self.parameters()?;
                self.expr_next(spanned!(
                    e.span.start..span.end,
                    Expr::Call(Box::new(e), pl)
                ))
            }
            TokenKind::BracketOpen => {
                self.advance();
                let ix = self.expr()?;
                let t = self.consume_next(TokenKind::BracketClose)?;
                self.expr_next(spanned!(
                    e.span.start..t.span.end,
                    Expr::Array(Box::new(e), Box::new(ix))
                ))
            }
            TokenKind::Dot => {
                self.advance();
                let id = self.consume_next(TokenKind::Ident)?;
                let i = self.text(id).to_string();
                self.expr_next(spanned!(
                    e.span.start..id.span.end,
                    Expr::Field(Box::new(e), i)
                ))
            }

            x if is_binop(&x) => {
                let bop = self.next()?;
                let e2 = self.expr()?;
                let x = make_binop(bop.kind, e, e2);
                if let Expr::Binop(TokenKind::Assign, ref e, _) = x.node {
                    if !can_assign(&e.node) {
                        return Err(SyntaxError::InvalidAssignment(x));
                    }
                }

                Ok(x)
            }

            _ => {
                if let Some(ep) = self.expr_short()? {
                    // handle no parenthesis call
                    fn parse(e: SpanExpr, ep: Spanned<Expr>) -> SpanExpr {
                        let p = ep.span;
                        match ep.node {
                            Expr::Apply(e2, l) => {
                                let mut l2 = vec![*e2];
                                l2.extend_from_slice(&l);
                                spanned!(e.span.start..p.end, Expr::Apply(Box::new(e), l2))
                            }
                            Expr::Binop(op, e1, e2) => {
                                spanned!(
                                    e.span.start..p.end,
                                    Expr::Binop(op, Box::new(parse(e, *e1)), e2)
                                )
                            }
                            _ => spanned!(p, Expr::Apply(Box::new(e), vec![ep])),
                        }
                    }

                    Ok(parse(e, ep))
                } else {
                    Ok(e)
                }
            }
        }
    }

    fn parse_list<F, R>(
        &mut self,
        sep: TokenKind,
        stop: TokenKind,
        mut parse: F,
    ) -> ParseResult<Vec<R>>
    where
        F: FnMut(&mut Self) -> ParseResult<R>,
    {
        let mut data = vec![];
        let mut comma = true;

        while self.peek() != stop && self.peek() != TokenKind::Eof {
            if !comma {
                return Err(SyntaxError::UnexpectedToken {
                    expected: format!("{}", sep),
                    got: self.next()?,
                });
            }

            let entry = parse(self)?;
            data.push(entry);

            comma = self.peek() == sep;
            if comma {
                self.advance();
            }
        }

        self.consume(stop)?;

        Ok(data)
    }

    fn parameters(&mut self) -> ParseResult<(Span, Vec<SpanExpr>)> {
        let mut args = vec![];
        while !self.at(TokenKind::ParenClose) {
            args.push(self.expr()?);

            match self.peek() {
                TokenKind::ParenClose => break,
                TokenKind::Comma => self.advance(),
                _ => {
                    let token = self.next()?;
                    return Err(SyntaxError::UnexpectedToken {
                        expected: "',' or ')'".to_string(),
                        got: token,
                    });
                }
            }
        }
        let end = self.consume_next(TokenKind::ParenClose)?;
        Ok((end.span, args))
    }

    pub fn parse_lit(&mut self) -> ParseResult<SpanExpr> {
        let tok = self.next()?;
        match tok.kind {
            TokenKind::Int => Ok(spanned!(
                tok.span,
                Expr::Int(
                    self.text(tok)
                        .parse()
                        .map_err(|_| { SyntaxError::InvalidLiteral(tok) })?
                )
            )),
            TokenKind::Float => Ok(spanned!(
                tok.span,
                Expr::Float(
                    self.text(tok)
                        .parse()
                        .map_err(|_| { SyntaxError::InvalidLiteral(tok) })?
                )
            )),
            TokenKind::Str => Ok(spanned!(
                tok.span,
                Expr::Str(Self::parse_string(tok, self.text(tok))?)
            )),
            TokenKind::Ident => Ok(spanned!(tok.span, Expr::Ident(self.text(tok).to_string()))),
            TokenKind::Builtin => Ok(spanned!(
                tok.span,
                Expr::Builtin(self.text(tok).to_string())
            )),
            TokenKind::True => Ok(spanned!(tok.span, Expr::True)),
            TokenKind::False => Ok(spanned!(tok.span, Expr::False)),
            TokenKind::Null => Ok(spanned!(tok.span, Expr::Null)),
            TokenKind::This => Ok(spanned!(tok.span, Expr::This)),
            _ => unreachable!(),
        }
    }
    pub fn expr_short(&mut self) -> ParseResult<Option<SpanExpr>> {
        let tok = self.peek();
        match tok {
            x if is_binop(&x) => {
                let tok = self.next()?;
                if !is_unop(x) {
                    return ParseResult::Err(SyntaxError::UnexpectedToken {
                        expected: "unary operator".to_string(),
                        got: tok,
                    });
                }

                let e = self.expr()?;

                self.expr_next(make_unop(tok.kind, e)).map(|x| Some(x))
            }
            x if is_lit(x) => {
                let e = self.parse_lit()?;
                self.expr_next(e).map(|x| Some(x))
            }
            TokenKind::BracketOpen => {
                let tok = self.next()?;
                let ls = self.parse_list(TokenKind::Comma, TokenKind::BracketClose, Self::expr)?;
                self.expr_next(spanned!(tok.span, Expr::ArrayInit(ls)))
                    .map(|x| Some(x))
            }
            TokenKind::Semicolon => Ok(None),
            _ => Ok(None),
        }
    }

    fn parse_block(&mut self, span: Span) -> ParseResult<SpanExpr> {
        let mut exprs = vec![];
        while !self.at(TokenKind::BraceClose) {
            let e = self.expr()?;
            println!("{}", e);
            exprs.push(e);

            if self.at(TokenKind::Semicolon) {
                self.advance();
            } else {
                break;
            }
        }
        let end = self.consume_next(TokenKind::BraceClose)?;

        Ok(spanned!(span.start..end.span.end, Expr::Block(exprs)))
    }

    pub fn parse_program(&mut self) -> ParseResult<Vec<SpanExpr>> {
        let mut e = vec![];
        loop {
            match self.peek() {
                TokenKind::Eof => break,
                TokenKind::Semicolon => {
                    self.advance();
                }
                _ => e.push(self.expr()?),
            }
        }
        Ok(e)
    }
}
