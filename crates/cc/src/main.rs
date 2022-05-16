use cc::ast::Token;
use logos::Lexer;

fn main() {
    let mut lexer = Lexer::<Token>::new("$foo += 42");
    while let Some(tok) = lexer.next() {
        println!("{:?}", tok);
    }
}
