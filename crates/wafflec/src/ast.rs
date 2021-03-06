use std::ops::{Index, Range};

use logos::{Logos, SpannedIter};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenData<T> {
    pub line: usize,
    pub column: usize,
    pub length: usize,
    pub data: T,
}

/*
pub fn token_data(lex: &mut Lexer<TokenKind>) -> TokenData<()> {
    lex.extras.pos.column += lex.slice().len();
    let line = lex.extras.pos.line;
    let column = lex.extras.pos.column;
    let length = lex.slice().len();

    TokenData {
        line,
        column,
        length,
        data: (),
    }
}*/

#[derive(Default)]
pub struct Extras {
    pos: Position,
}

#[derive(Debug, Clone, PartialEq, Logos, Hash, Eq, Copy, Display)]
#[logos(extras = Extras)]
pub enum TokenKind {
    #[token("(")]
    #[display(fmt = "(")]
    ParenOpen,
    #[regex(r"[ \t\f]", |lex| {
        lex.extras.pos.column += lex.slice().len();
        logos::Skip
    })]
    #[regex( r"\n", |lex| {
        lex.extras.pos.line += 1;
        lex.extras.pos.column = 0;

        logos::Skip
    })]
    Eof,
    #[error]
    Error,
    #[token(";")]
    #[display(fmt = ";")]
    Semicolon,
    #[display(fmt = ".")]
    #[token(".")]
    Dot,
    #[display(fmt = ",")]
    #[token(",")]
    Comma,
    #[display(fmt = "->")]
    #[token("->")]
    Arrow,
    #[display(fmt = "{{")]
    #[token("{")]
    BraceOpen,
    #[display(fmt = "}}")]
    #[token("}")]
    BraceClose,

    #[display(fmt = ")")]
    #[token(")")]
    ParenClose,
    #[display(fmt = "[")]
    #[token("[")]
    BracketOpen,
    #[display(fmt = "]")]
    #[token("]")]
    BracketClose,
    #[display(fmt = "true")]
    #[token("true")]
    True,

    #[display(fmt = "false")]
    #[token("false")]
    False,
    #[display(fmt = "null")]
    #[token("null")]
    Null,
    #[display(fmt = "this")]
    #[token("this")]
    This,
    #[display(fmt = "integer literal")]
    #[regex(r"\d+", |lex| {
        lex.extras.pos.column += lex.slice().len();
    })]
    Int,
    #[display(fmt = "float literal")]
    #[regex(r"\d+\.\d+", |lex| {
        lex.extras.pos.column += lex.slice().len()
    })]
    Float,

    #[display(fmt = "string literal")]
    #[regex(r#""[^"]*""#, |lex| {
        lex.extras.pos.column += lex.slice().len();
    })]
    Str,
    #[display(fmt = "builtin")]
    #[regex(r"\$[a-zA-Z_][a-zA-Z0-9_]*", |lex| {
        lex.extras.pos.column += lex.slice().len();
    })]
    Builtin,

    #[token("new")]
    #[display(fmt = "new")]
    New,
    #[display(fmt = "identifier")]
    // identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| {
        lex.extras.pos.column += lex.slice().len();
    })]
    Ident,
    #[display(fmt = "var")]
    #[token("var")]
    Var,
    #[display(fmt = "while")]
    #[token("while")]
    While,
    #[display(fmt = "do")]
    #[token("do")]
    Do,
    #[display(fmt = "if")]
    #[token("if")]
    If,
    #[display(fmt = "else")]
    #[token("else")]
    Else,
    #[display(fmt = "fun")]
    #[token("fun")]
    Function,
    #[display(fmt = "return")]
    #[token("return")]
    Return,
    #[display(fmt = "break")]
    #[token("break")]
    Break,
    #[display(fmt = "continue")]
    #[token("continue")]
    Continue,
    #[display(fmt = "default")]
    #[token("default")]
    Default,
    #[display(fmt = "try")]
    #[token("try")]
    Try,

    #[display(fmt = "in")]
    #[token("in")]
    In,
    #[display(fmt = "let")]
    #[token("let")]
    Let,
    #[display(fmt = "catch")]
    #[token("catch")]
    Catch,
    #[display(fmt = "match")]
    #[token("match")]
    Switch,
    #[display(fmt = "then")]
    #[token("then")]
    Then,
    #[display(fmt = "rec")]
    #[token("rec")]
    Rec,
    #[display(fmt = "+")]
    #[token("+")]
    Plus,
    #[display(fmt = "-")]
    #[token("-")]
    Minus,
    #[display(fmt = "*")]
    #[token("*")]
    Star,
    #[display(fmt = "/")]
    #[token("/")]
    Slash,
    #[display(fmt = "%")]
    #[token("%")]
    Percent,

    #[display(fmt = ">")]
    #[token(">")]
    Greater,
    #[display(fmt = ">=")]
    #[token(">=")]
    GreaterEq,
    #[display(fmt = "<")]
    #[token("<")]
    Less,
    #[display(fmt = "<=")]
    #[token("<=")]
    LessEq,
    #[display(fmt = "==")]
    #[token("==")]
    Eq,
    #[display(fmt = "!=")]
    #[token("!=")]
    Neq,
    #[display(fmt = ">>")]
    #[token(">>")]
    Shr,
    #[display(fmt = "<<")]
    #[token("<<")]
    Shl,
    #[display(fmt = ">>>")]
    #[token(">>>")]
    UShr,
    #[display(fmt = "&")]
    #[token("&")]
    BitAnd,
    #[display(fmt = "|")]
    #[token("|")]
    BitOr,
    #[display(fmt = "^")]
    #[token("^")]
    Xor,
    #[display(fmt = "and")]
    #[token("and")]
    And,
    #[display(fmt = "||")]
    #[token("||")]
    Or,
    #[display(fmt = "<=>")]
    #[token("<=>")]
    Spaceship,
    #[display(fmt = "&&")]
    #[token("&&")]
    Band,

    #[token("+=")]
    PlusAssign,
    #[token("-=")]
    MinusAssign,
    #[token("*=")]
    StarAssign,
    #[token("/=")]
    SlashAssign,
    #[token("%=")]
    PercentAssign,
    #[token(">>=")]
    ShrAssign,
    #[token("<<=")]
    ShlAssign,
    #[token(">>>=")]
    UShrAssign,
    #[token("&=")]
    BitAndAssign,
    #[token("|=")]
    BitOrAssign,
    #[token("^=")]
    XorAssign,
    #[display(fmt = "=")]
    #[token("=")]
    Assign,
    #[display(fmt = "!")]
    #[token("!")]
    Not,
    #[token(":")]
    Colon,
    #[token("import")]
    Import,
    #[token("export")]
    Export,
    #[token("method")]
    Method,
    #[token("inherit")]
    #[display(fmt = "inherit")]
    Inherit,
    #[token("class")]
    #[display(fmt = "class")]
    Class,
    #[token("object")]
    #[display(fmt = "object")]
    Object,
    #[display(fmt = "val")]
    #[token("val")]
    Val,
    #[display(fmt = "initializer")]
    #[token("initializer")]
    Initializer,
}

fn opt(x: &Option<impl ToString>) -> String {
    match x {
        Some(x) => x.to_string(),
        None => "<none>".to_string(),
    }
}

fn join(vec: &[impl ToString]) -> String {
    vec.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(" ")
}

fn join2(vec: &[impl ToString], sep: &str) -> String {
    vec.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(sep)
}
use derive_more::Display;
#[derive(Debug, Display, Clone, PartialEq)]
pub enum Expr {
    #[display(fmt = "{}", _0)]
    Float(f64),
    Int(i64),
    Str(String),
    Builtin(String),
    Ident(String),
    True,
    False,
    Null,
    This,
    #[display(fmt = "(block [{}]])", "join(_0)")]
    Block(Vec<Spanned<Expr>>),
    #[display(fmt = "(array [{}])", "join(_0)")]
    ArrayInit(Vec<Spanned<Expr>>),
    #[display(fmt = "({})", _0)]
    Parenthesis(Box<Spanned<Expr>>),
    #[display(fmt = "{}.{}", _0, _1)]
    Field(Box<Spanned<Expr>>, String),
    #[display(fmt = "(call {} :args [{}])", _0, "join(_1)")]
    Call(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    #[display(fmt = "(apply {} :args [{}])", _0, "join(_1)")]
    Apply(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    #[display(fmt = "{}[{}]", _0, _1)]
    Array(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    #[display(fmt = "vars")]
    Vars(Vec<(String, Option<Box<Spanned<Expr>>>)>),
    #[display(fmt = "{}", "print_let(_0,*_2,_1)")]
    Let(Vec<Spanned<LetDef>>, Option<Box<Spanned<Expr>>>, bool),

    #[display(fmt = "(while :cond {} :body {} :do {})", _0, _1, _2)]
    While(Box<Spanned<Expr>>, Box<Spanned<Expr>>, bool),
    #[display(fmt = "(if :cond {} :then {} :else {})", _0, _1, "opt(_2)")]
    If(
        Box<Spanned<Expr>>,
        Box<Spanned<Expr>>,
        Option<Box<Spanned<Expr>>>,
    ),
    #[display(fmt = "(try :body {} :catch {} {})", _0, _1, _2)]
    Try(Box<Spanned<Expr>>, String, Box<Spanned<Expr>>),
    #[display(fmt = "(fun :args ({}) :body [{}])", "join(_1)", _2)]
    Function(Option<String>, Vec<String>, Box<Spanned<Expr>>),
    #[display(fmt = "(binop :op {} :lhs {} :rhs {})", _0, _1, _2)]
    Binop(TokenKind, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    #[display(fmt = "(unop :op {} :expr {})", _0, _1)]
    Unop(TokenKind, Box<Spanned<Expr>>),
    #[display(fmt = "(return {})", "opt(_0)")]
    Return(Option<Box<Spanned<Expr>>>),
    #[display(fmt = "(break {})", "opt(_0)")]
    Break(Option<Box<Spanned<Expr>>>),
    Continue,
    #[display(fmt = "(next {}::{})", _0, _1)]
    Next(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    #[display(fmt = "(object {})", "tuplea(_0)")]
    Object(Vec<(String, Spanned<Expr>)>),
    Label(String),
    #[display(fmt = "match")]
    Switch(
        Box<Spanned<Expr>>,
        Vec<(Spanned<Pattern>, Option<Spanned<Expr>>, Spanned<Expr>)>,
        Option<Box<Spanned<Expr>>>,
    ),
    #[display(fmt = "import {}", "print_import(_0)")]
    Import(Vec<String>),
    ImportStr(String),
    #[display(fmt = "export [{}]", "join(_0)")]
    Export(Vec<Export>),
    #[display(fmt = "new {}({})", _0, "join(_1)")]
    New(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    #[display(fmt = "class {} {} = {}", _0, "join(_1)", _2)]
    Class(String, Vec<Box<str>>, Box<Spanned<ClassExpr>>),
}

#[derive(Debug, Display, Clone, PartialEq)]
pub enum ClassExpr {
    #[display(fmt = "{}", "print_let(_0,*_2,_1)")]
    Let(Vec<Spanned<LetDef>>, Option<Box<Spanned<ClassExpr>>>, bool),
    #[display(fmt = "object {{ {} }}", "join2(_0, \";\")")]
    Object(Vec<Spanned<ClassField>>),
}
#[derive(Debug, Display, Clone, PartialEq)]
pub enum ClassField {
    #[display(fmt = "inherit {} ({})", _0, "join(_1)")]
    Inherit(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    #[display(fmt = "val {} = {}", _0, _1)]
    Val(String, Box<Spanned<Expr>>),
    #[display(fmt = "method {} {} = {}", _0, "join(_1)", _2)]
    Method(String, Vec<Box<str>>, Box<Spanned<Expr>>),
    #[display(fmt = "initializer {}", _0)]
    Initializer(Box<Spanned<Expr>>),
}

#[derive(Debug, Display, Clone, PartialEq)]
pub enum Export {
    /// export { <identifier> as <identifier> }
    #[display(fmt = "{} as {}", _0, _1)]
    Alias(String, String),
    /// export { <identifier> }
    #[display(fmt = "{}", _0)]
    Identifier(String),
}

fn print_import(x: &[String]) -> String {
    let mut f = String::new();
    for (i, x) in x.iter().enumerate() {
        f.push_str(x);
        if i != x.len() - 1 {
            f.push('.');
        }
    }
    f
}

fn print_let<T: std::fmt::Display + std::fmt::Debug + Clone + PartialEq>(
    defs: &[Spanned<LetDef>],
    rec: bool,
    body: &Option<Box<Spanned<T>>>,
) -> String {
    let mut f = String::new();
    f.push_str("let ");
    if rec {
        f.push_str("rec ");
    }
    for (i, def) in defs.iter().enumerate() {
        match &def.node {
            LetDef::Variable(name, expr) => {
                f.push_str(&format!("{} = {}", name, expr));
            }
            LetDef::Function(name, params, expr) => {
                let args = if params.is_empty() {
                    "()".to_string()
                } else {
                    join(params)
                };
                f.push_str(&format!("{} {} = {}", name, args, expr))
            }
        }
        if i != defs.len() - 1 {
            f.push_str("\nand ");
        }
    }
    if let Some(body) = body {
        f.push_str("\nin\n");
        f.push_str(&format!("\t{}", body));
    }

    f
}

fn tuplea(p: &[(impl ToString, impl ToString)]) -> String {
    let mut f = String::new();
    f.push_str("{ ");
    for (i, (field, pat)) in p.iter().enumerate() {
        f.push_str(&field.to_string());
        f.push(':');
        f.push(' ');
        f.push_str(&pat.to_string());
        if i != p.len() - 1 {
            f.push(',');
        }
    }
    f.push_str(" }");
    f
}
fn tupleb(p: &[(impl ToString, Option<impl ToString>)]) -> String {
    let mut f = String::new();
    f.push_str("{ ");
    for (i, (field, pat)) in p.iter().enumerate() {
        f.push_str(&field.to_string());

        if let Some(ref pat) = pat {
            f.push(':');
            f.push(' ');
            f.push_str(&pat.to_string());
        }

        if i != p.len() - 1 {
            f.push(',');
        }
    }
    f.push_str(" }");
    f
}
#[derive(Debug, Display, Clone, PartialEq)]
pub enum Pattern {
    #[display(fmt = "{}", _0)]
    Ident(String),
    #[display(fmt = "{}", _0)]
    Int(i64),
    #[display(fmt = "{}", _0)]
    Float(f64),
    #[display(fmt = "{}", _0)]
    Bool(bool),
    Str(String),
    #[display(fmt = "null")]
    Null,
    #[display(fmt = "{} as {}", _0, _1)]
    Alias(String, Box<Spanned<Pattern>>),
    #[display(fmt = "{}", _0)]
    Symbol(String),
    #[display(fmt = "{}", "tupleb(_0)")]
    Record(Vec<(String, Option<Spanned<Pattern>>)>),
    #[display(fmt = "[{}]", "join(_0)")]
    Array(Vec<Spanned<Pattern>>),

    Default,
}

impl<T> TokenData<T> {
    pub fn pos(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }
}

pub fn can_swap(op: &TokenKind, op1: &TokenKind) -> bool {
    let p1 = priority(op);
    let p2 = priority(op1);
    p1 <= p2
}

pub fn priority(op: &TokenKind) -> i32 {
    use TokenKind::*;
    match op {
        Assign | PlusAssign | MinusAssign | StarAssign | SlashAssign | BitAndAssign
        | BitOrAssign | XorAssign | PercentAssign => -4,
        Band | Or => -2,
        Eq | Neq | Greater | GreaterEq | LessEq | Less => -1,
        Plus | Minus => 0,
        Slash => 1,
        BitAnd | BitOr | Xor => 2,
        Shr | Shl | Percent | UShr => 3,
        Star => 1,
        _ => 4,
    }
}

pub fn is_binop(t: &TokenKind) -> bool {
    use TokenKind::*;
    match t {
        Assign | PlusAssign | MinusAssign | StarAssign | SlashAssign | BitAndAssign
        | BitOrAssign | XorAssign | PercentAssign => true,
        Or => true,
        Eq | Neq | Greater | GreaterEq | LessEq | Less => true,
        Plus | Minus => true,
        Star | Slash => true,
        BitAnd | BitOr | Xor => true,
        Shr | Shl | Percent | UShr => true,
        Not => true,
        _ => false,
    }
}

pub fn is_lit(t: TokenKind) -> bool {
    use TokenKind::*;
    match t {
        This | Null | True | False | Int | Float | Str | Builtin | Ident => true,
        _ => false,
    }
}

pub fn can_assign(e: &Expr) -> bool {
    use Expr::*;
    match e {
        Ident(_) | Field(_, _) | This | Array(_, _) => true,
        _ => false,
    }
}

pub fn make_binop(op: TokenKind, e: Spanned<Expr>, e2: Spanned<Expr>) -> Spanned<Expr> {
    match e2.node {
        Expr::Binop(_op, _e, _e2) if can_swap(&_op, &op) => {
            let _e = make_binop(op, e, *_e);
            let sp = e2.span;
            Spanned {
                node: Expr::Binop(_op, Box::new(_e), _e2),
                span: sp,
            }
        }
        _ => Spanned {
            span: e.span,
            node: Expr::Binop(op, Box::new(e), Box::new(e2)),
        },
    }
}

pub fn make_unop(op: TokenKind, e: Spanned<Expr>) -> Spanned<Expr> {
    let p = e.span;
    match e.node {
        Expr::Binop(bop, e, e2) => Spanned {
            span: p,
            node: Expr::Binop(bop, Box::new(make_unop(op, *e)), e2),
        },
        _ => Spanned {
            span: p,
            node: Expr::Unop(op, Box::new(e)),
        },
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq)]
#[display(fmt = "{}", kind)]
/// Holds the kind of token for parsing, and the span to extract it's text from the source code
pub struct Token {
    /// The type of token
    pub kind: TokenKind,
    /// The position of the `Token` in the source code
    pub span: Span,
}

impl Token {
    #[inline]
    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span]
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq)]
#[display(fmt = "{}..{}", start, end)]
/// Custom span for storing the position of a token or AST node in the source string
pub struct Span {
    /// The start of the span (inclusive)
    pub start: usize,
    /// The end of the span (exclusive)
    pub end: usize,
}

impl<'a> From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

impl<'a> From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl<'a> Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}

/// A wrapper around `logos::SpannedIter` to map to our custom `Token` type and also to map `None`
/// to `TK::Eof` to allow for easier Eof handling while parsing
pub struct Lexer<'input> {
    /// The length of the input string so the EOF `Token` can have a correct span
    length: usize,
    logos: SpannedIter<'input, TokenKind>,
    eof: bool,
}

impl<'input> Lexer<'input> {
    /// Create a new `Lexer` that lazily lexes `input`
    pub fn new(input: &'input str) -> Self {
        Self {
            length: input.len(),
            logos: TokenKind::lexer(input).spanned(),
            eof: false,
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.logos.next() {
            Some((kind, span)) => Some(Token {
                kind,
                span: span.into(),
            }),
            None if self.eof => None,
            None => {
                self.eof = true;
                Some(Token {
                    kind: TokenKind::Eof,
                    span: (self.length..self.length).into(),
                })
            }
        }
    }
}

/// Generic struct that allows you to attach a `Span` to `T`
#[derive(Debug, Display, Clone, PartialEq)]
#[display(fmt = "{}", node)]
pub struct Spanned<T>
where
    T: std::fmt::Debug + std::fmt::Display + Clone + PartialEq,
{
    pub span: Span,
    pub node: T,
}

pub fn is_unop(x: TokenKind) -> bool {
    match x {
        TokenKind::Not | TokenKind::Minus | TokenKind::BitAnd | TokenKind::Star => true,
        _ => false,
    }
}

#[derive(Debug, Clone, PartialEq, Display)]
pub enum LetDef {
    #[display(fmt = "function")]
    Function(String, Vec<String>, Box<Spanned<Expr>>),
    #[display(fmt = "variable")]
    Variable(Spanned<Pattern>, Box<Spanned<Expr>>),
}
