use logos::{Lexer, Logos};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenData<T> {
    pub line: usize,
    pub column: usize,
    pub length: usize,
    pub data: T,
}

pub fn token_data(lex: &mut Lexer<Token>) -> TokenData<()> {
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
}

#[derive(Default)]
pub struct Extras {
    pos: Position,
}

#[derive(Debug, Clone, PartialEq, Logos)]
#[logos(extras = Extras)]
pub enum Token {
    #[regex(r"[ \t\f]", |lex| {
        lex.extras.pos.column += lex.slice().len();
        logos::Skip
    })]
    #[regex( r"\n", |lex| {
        lex.extras.pos.line += 1;
        lex.extras.pos.column = 0;

        logos::Skip
    })]
    #[error]
    Eof,
    #[token(";", token_data)]
    Semicolon(TokenData<()>),
    #[token(".", token_data)]
    Dot(TokenData<()>),
    #[token(",", token_data)]
    Comma(TokenData<()>),
    #[token("->", token_data)]
    Arrow(TokenData<()>),
    #[token("{", token_data)]
    BraceOpen(TokenData<()>),
    #[token("}", token_data)]
    BraceClose(TokenData<()>),
    #[token("(", token_data)]
    ParenOpen(TokenData<()>),
    #[token(", token_data)", token_data)]
    ParenClose(TokenData<()>),
    #[token("[", token_data)]
    BracketOpen(TokenData<()>),
    #[token("]", token_data)]
    BracketClose(TokenData<()>),
    #[token("true", token_data)]
    True(TokenData<()>),
    #[token("false", token_data)]
    False(TokenData<()>),
    #[token("null", token_data)]
    Null(TokenData<()>),
    #[token("this", token_data)]
    This(TokenData<()>),
    #[regex(r"\d+", |lex| {
        lex.extras.pos.column += lex.slice().len();
        let line = lex.extras.pos.line;
        let column = lex.extras.pos.column;
        let length = lex.slice().len();

        TokenData {line,column,length,data:lex.slice().parse::<i64>().unwrap()}
    })]
    Int(TokenData<i64>),
    #[regex(r"\d+\.\d+", |lex| {
        lex.extras.pos.column += lex.slice().len();
        let line = lex.extras.pos.line;
        let column = lex.extras.pos.column;
        let length = lex.slice().len();

        TokenData {line,column,length,data:lex.slice().parse::<f64>().unwrap()}
    })]
    Float(TokenData<f64>),
    #[regex(r#""[^"]*""#, |lex| {
        lex.extras.pos.column += lex.slice().len();
        let line = lex.extras.pos.line;
        let column = lex.extras.pos.column;
        let length = lex.slice().len();
        TokenData {line,column,length,data:lex.slice().to_string()}
    })]
    Str(TokenData<String>),
    #[regex(r"\$[a-zA-Z_][a-zA-Z0-9_]*", |lex| {
        lex.extras.pos.column += lex.slice().len();
        let line = lex.extras.pos.line;
        let column = lex.extras.pos.column;
        let length = lex.slice().len();
        TokenData {line,column,length,data:lex.slice().to_string()}
    })]
    Builtin(TokenData<String>),
    // identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| {
        lex.extras.pos.column += lex.slice().len();
        let line = lex.extras.pos.line;
        let column = lex.extras.pos.column;
        let length = lex.slice().len();
        TokenData {line,column,length,data:lex.slice().to_string()}
    })]
    Ident(TokenData<String>),
    #[token("var", token_data)]
    Var(TokenData<()>),
    #[token("while", token_data)]
    While(TokenData<()>),
    #[token("do", token_data)]
    Do(TokenData<()>),
    #[token("if", token_data)]
    If(TokenData<()>),
    #[token("else", token_data)]
    Else(TokenData<()>),
    #[token("function", token_data)]
    Function(TokenData<()>),
    #[token("return", token_data)]
    Return(TokenData<()>),
    #[token("break", token_data)]
    Break(TokenData<()>),
    #[token("continue", token_data)]
    Continue(TokenData<()>),
    #[token("default", token_data)]
    Default(TokenData<()>),
    #[token("try", token_data)]
    Try(TokenData<()>),
    #[token("catch", token_data)]
    Catch(TokenData<()>),
    #[token("switch", token_data)]
    Switch(TokenData<()>),
    #[token("+", token_data)]
    Plus(TokenData<()>),
    #[token("-", token_data)]
    Minus(TokenData<()>),
    #[token("*", token_data)]
    Star(TokenData<()>),
    #[token("/", token_data)]
    Slash(TokenData<()>),
    #[token("%", token_data)]
    Percent(TokenData<()>),

    #[token(">", token_data)]
    Greater(TokenData<()>),
    #[token(">=", token_data)]
    GreaterEq(TokenData<()>),
    #[token("<", token_data)]
    Less(TokenData<()>),
    #[token("<=", token_data)]
    LessEq(TokenData<()>),
    #[token("==", token_data)]
    Eq(TokenData<()>),
    #[token("!=", token_data)]
    Neq(TokenData<()>),
    #[token(">>", token_data)]
    Shr(TokenData<()>),
    #[token("<<", token_data)]
    Shl(TokenData<()>),
    #[token(">>>", token_data)]
    UShr(TokenData<()>),
    #[token("&", token_data)]
    BitAnd(TokenData<()>),
    #[token("|", token_data)]
    BitOr(TokenData<()>),
    #[token("^", token_data)]
    Xor(TokenData<()>),
    #[token("&&", token_data)]
    And(TokenData<()>),
    #[token("||", token_data)]
    Or(TokenData<()>),
    #[token("<=>", token_data)]
    Spaceship(TokenData<()>),

    #[token("+=", token_data)]
    PlusAssign(TokenData<()>),
    #[token("-=", token_data)]
    MinusAssign(TokenData<()>),
    #[token("*=", token_data)]
    StarAssign(TokenData<()>),
    #[token("/=", token_data)]
    SlashAssign(TokenData<()>),
    #[token("%=", token_data)]
    PercentAssign(TokenData<()>),
    #[token(">>=", token_data)]
    ShrAssign(TokenData<()>),
    #[token("<<=", token_data)]
    ShlAssign(TokenData<()>),
    #[token(">>>=", token_data)]
    UShrAssign(TokenData<()>),
    #[token("&=", token_data)]
    BitAndAssign(TokenData<()>),
    #[token("|=", token_data)]
    BitOrAssign(TokenData<()>),
    #[token("^=", token_data)]
    XorAssign(TokenData<()>),
    #[token("=", token_data)]
    Assign(TokenData<()>),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Binop {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Neq,
    Shr,
    Shl,
    UShr,
    BitAnd,
    BitOr,
    Xor,
    And,
    Or,
    Spaceship,

    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    ShrAssign,
    ShlAssign,
    UShrAssign,
    BitAndAssign,
    BitOrAssign,
    XorAssign,

    Assign,
}

#[derive(Debug, Clone)]
pub enum ExprDecl {
    Float(f64),
    Int(i64),
    Str(String),
    Builtin(String),
    True,
    False,
    Null,
    This,
    Block(Vec<Expr>),
    Parenthesis(Box<Expr>),
    Field(Box<Expr>, String),
    Call(Box<Expr>, Vec<Expr>),
    Array(Box<Expr>, Box<Expr>),
    Vars(Vec<(String, Option<Box<Expr>>)>),
    While(Box<Expr>, Box<Expr>, bool),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Try(Box<Expr>, String, Box<Expr>),
    Function(Option<String>, Vec<String>, Box<Expr>),
    Binop(Binop, Box<Expr>, Box<Expr>),
    Return(Option<Box<Expr>>),
    Break(Option<Box<Expr>>),
    Continue,
    Next(Box<Expr>, Box<Expr>),
    Object(Vec<(String, Expr)>),
    Label(String),
    Switch(Box<Expr>, Vec<(Expr, Expr)>, Option<Box<Expr>>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub pos: Position,
    pub decl: ExprDecl,
}
