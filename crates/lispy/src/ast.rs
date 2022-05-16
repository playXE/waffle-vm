#[derive(Clone, PartialEq, Debug)]
pub enum ExprKind {
    /// (define (foo a b c . d) <expr>) or (lambda (a b c . d ) <expr>)
    Fn(Option<Box<str>>, bool, Box<[Box<str>]>, Box<Expr>),
    Def(Box<str>, Option<Box<Expr>>),
    Var(Box<str>),
    // let behaves as `let*` in our compiler
    Let(Box<[(Box<str>, Box<Expr>)]>, Box<Expr>),
    Letrec(Box<[(Box<str>, Box<Expr>)]>, Box<Expr>),
    NamedLet(Box<str>, Box<[(Box<str>, Box<Expr>)]>, Box<Expr>),
    NamedLetrec(Box<str>, Box<[(Box<str>, Box<Expr>)]>, Box<Expr>),
    Int(i64),
    Float(f64),
    Bool(bool),
    Null,
    Str(Box<str>),
    Symbol(Box<str>),
    /// (. field object)
    Field(Box<Expr>, Box<[Box<str>]>),
    /// set!
    Set(Box<Expr>, Box<Expr>),
    /// (<expr> <args>)
    Call(Box<Expr>, Box<[Box<Expr>]>),
    /// (.- method object <args>)
    MethodCall(Box<str>, Box<Expr>, Box<[Box<Expr>]>),
    Constructor(Box<Expr>, Box<[Box<Expr>]>),
    /// (begin <exprs>)
    Begin(Box<[Box<Expr>]>),
    While(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    ArrayInit(Box<[Box<Expr>]>),

    Import(Vec<Import>),
    Export(Vec<String>),
}
#[derive(Clone, PartialEq, Debug)]
pub struct Expr {
    pub kind: ExprKind,
}
#[derive(Clone, PartialEq, Debug)]
pub enum Import {
    Module(Vec<String>),
    File(String),
}
