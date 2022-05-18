use std::collections::HashSet;
use std::rc::Rc;

use waffle::opcode::*;
use waffle::reflect::*;

use crate::ast::Expr;
use crate::ast::LetDef;
use crate::ast::Pattern;
use crate::ast::TokenKind;
use crate::parser::SpanExpr;

pub struct Compiler<'a, 'b> {
    pub ctx: &'b mut Context<'a>,
    pub toplevel: bool,
    pub refvars: &'b mut HashSet<String>,
}

impl Compiler<'_, '_> {
    fn add_vars_for_dassign(&mut self, p: &Pattern) {
        match p {
            Pattern::Array(pats) => {
                for pat in pats.iter() {
                    self.add_vars_for_dassign(&pat.node);
                }
            }
            Pattern::Record(rec) => {
                for (name, p) in rec.iter() {
                    match p.as_ref().map(|x| &x.node) {
                        Some(Pattern::Ident(real_name)) => {
                            self.ctx.add_var(real_name, false);
                        }
                        None => self.ctx.add_var(name, false),
                        _ => unreachable!(),
                    }
                }
            }
            Pattern::Ident(name) => self.ctx.add_var(name, false),
            _ => unreachable!(),
        }
    }

    pub fn destructuring_assign(&mut self, p: &Pattern) {
        match p {
            Pattern::Ident(x) => {
                let a = self.ctx.access_var(x);
                self.ctx.compile_access_set(&a);
            }
            Pattern::Array(arr) => {
                self.ctx.write(Op::Push);
                for (i, pat) in arr.iter().enumerate() {
                    self.ctx.write(Op::AccStack(0));
                    self.ctx.write(Op::AccIndex(i as _));
                    self.destructuring_assign(&pat.node);
                }
                self.ctx.write(Op::Pop(1));
            }
            Pattern::Record(rec) => {
                self.ctx.write(Op::Push);
                for (field, pat) in rec.iter() {
                    let g = self
                        .ctx
                        .global(Rc::new(Global::Symbol(field.clone().into_boxed_str())))
                        as i32;
                    match pat {
                        Some(pat) => {
                            self.ctx.write(Op::AccStack(0));
                            self.ctx.write(Op::AccField(g));
                            self.destructuring_assign(&pat.node);
                        }
                        None => {
                            self.ctx.write(Op::AccStack(0));
                            self.ctx.write(Op::AccField(g));
                            let a = self.ctx.access_var(field);
                            self.ctx.compile_access_set(&a);
                        }
                    }
                }
                self.ctx.write(Op::Pop(1));
            }
            _ => unreachable!(),
        }
    }

    pub fn expr(&mut self, expr: &SpanExpr, tail: bool) {
        match &expr.node {
            Expr::Ident(x) if self.refvars.contains(x) => {
                self.ctx.use_var(x);
                self.ctx.write(Op::AccIndex(0));
            }
            Expr::Ident(x) => self.ctx.use_var(x),
            Expr::Builtin(x) => {
                let name = self.ctx.global(Rc::new(Global::Symbol(
                    x[1..].to_string().clone().into_boxed_str(),
                )));
                self.ctx.write(Op::AccBuiltin(name as _));
            }
            Expr::This => self.ctx.write(Op::AccThis),
            Expr::Null => self.ctx.write(Op::AccNull),
            Expr::Int(x) => {
                let x = *x;
                if x as i32 as i64 == x {
                    self.ctx.write(Op::AccInt(x as _));
                } else {
                    let g = self.ctx.global(Rc::new(Global::Int(x)));
                    self.ctx.write(Op::AccGlobal(g as _));
                }
            }
            Expr::Float(x) => {
                let g = self.ctx.global(Rc::new(Global::Float(x.to_bits())));
                self.ctx.write(Op::AccGlobal(g as _));
            }
            Expr::Str(x) => {
                let g = self
                    .ctx
                    .global(Rc::new(Global::Str(x.to_string().into_boxed_str())));
                self.ctx.write(Op::AccGlobal(g as _));
            }
            Expr::Field(obj, field) => {
                self.expr(obj, false);
                let g = self
                    .ctx
                    .global(Rc::new(Global::Symbol(field.clone().into_boxed_str())));
                self.ctx.write(Op::AccField(g as _));
            }
            Expr::Apply(callee, args) => {
                for arg in args.iter() {
                    self.expr(arg, false);
                    self.ctx.write(Op::Push);
                }
                match callee.node {
                    Expr::Field(ref object, ref method) => {
                        self.expr(&object, false);
                        self.ctx.write(Op::Push);
                        let g = self
                            .ctx
                            .global(Rc::new(Global::Symbol(method.clone().into_boxed_str())));
                        self.ctx.write(Op::AccField(g as _));
                        self.ctx.write(Op::ObjCall(args.len() as _));
                        return;
                    }
                    _ => self.expr(callee, false),
                }
                if tail {
                    self.ctx.write(Op::TailCall(
                        self.ctx.stack as i16 - self.ctx.limit as i16,
                        args.len() as _,
                    ))
                } else {
                    self.ctx.write(Op::Call(args.len() as _))
                }
            }
            Expr::Call(callee, args) => {
                for arg in args.iter() {
                    self.expr(arg, false);
                    self.ctx.write(Op::Push);
                }
                match callee.node {
                    Expr::Field(ref object, ref method) => {
                        self.expr(&object, false);
                        self.ctx.write(Op::Push);
                        let g = self
                            .ctx
                            .global(Rc::new(Global::Symbol(method.clone().into_boxed_str())));
                        self.ctx.write(Op::AccField(g as _));
                        self.ctx.write(Op::ObjCall(args.len() as _));
                        return;
                    }
                    _ => self.expr(callee, false),
                }
                if tail {
                    self.ctx.write(Op::TailCall(
                        self.ctx.stack as i16 - self.ctx.limit as i16,
                        args.len() as _,
                    ))
                } else {
                    self.ctx.write(Op::Call(args.len() as _))
                }
            }
            Expr::Return(expr) => {
                match expr {
                    Some(x) => self.expr(x, true),
                    None => self.ctx.write(Op::AccNull),
                }
                let stack = self.ctx.stack;
                for i in 0..self.ctx.traps.len() {
                    let trap = self.ctx.traps[i];
                    if self.ctx.stack > trap {
                        self.ctx.write(Op::Pop(self.ctx.stack as i32 - trap as i32));
                        self.ctx.write(Op::EndTrap);
                    }
                }
                self.ctx
                    .write(Op::Ret(self.ctx.stack as i32 - self.ctx.limit as i32));
                self.ctx.stack = stack;
            }
            Expr::Object(fields) => {
                self.ctx.write(Op::AccNull);
                self.ctx.write(Op::New);
                if !fields.is_empty() {
                    self.ctx.write(Op::Push);
                    for (field, expr) in fields.iter() {
                        self.ctx.write(Op::Push);
                        self.expr(expr, false);
                        let g = self
                            .ctx
                            .global(Rc::new(Global::Symbol(field.clone().into_boxed_str())));
                        self.ctx.write(Op::SetField(g as _));
                        self.ctx.write(Op::AccStack(0));
                    }
                    self.ctx.write(Op::Pop(1));
                }
            }
            Expr::Let(defs, body, recursive) => {
                let saved = if body.is_some() {
                    let locals = self.ctx.locals.clone();
                    let stack = self.ctx.stack;
                    let refvars = self.refvars.clone();
                    self.ctx.enter_scope();
                    Some((locals, stack, refvars))
                } else {
                    None
                };

                if *recursive {
                    for def in defs.iter() {
                        match &def.node {
                            LetDef::Variable(name, _) => {
                                self.add_vars_for_dassign(&name.node);
                                //self.ctx.add_var(name, false);
                            }
                            LetDef::Function(name, _, _) => {
                                //  self.ctx.write(Op::AccNull);
                                //self.ctx.write(Op::MakeArray(0));
                                self.ctx.add_var(name, false);
                                // self.refvars.insert(name.clone());
                            }
                        }
                    }

                    for def in defs.iter() {
                        match &def.node {
                            LetDef::Variable(name, value) => {
                                self.expr(value, false);
                                self.destructuring_assign(&name.node);
                            }
                            LetDef::Function(name, parameters, body) => {
                                self.compile_lambda(
                                    &parameters
                                        .iter()
                                        .map(|x| x.clone().into_boxed_str())
                                        .collect::<Vec<_>>(),
                                    body,
                                );

                                let acc = self.ctx.access_var(name);
                                self.ctx.compile_access_set(&acc);
                            }
                        }
                    }
                } else {
                    for def in defs.iter() {
                        match &def.node {
                            LetDef::Variable(name, value) => {
                                self.add_vars_for_dassign(&name.node);
                                self.expr(value, false);
                                self.destructuring_assign(&name.node);
                            }
                            LetDef::Function(name, parameters, body) => {
                                self.compile_lambda(
                                    &parameters
                                        .iter()
                                        .map(|x| x.clone().into_boxed_str())
                                        .collect::<Vec<_>>(),
                                    body,
                                );
                                self.ctx.add_var(name, true);
                            }
                        }
                    }
                }

                if let Some(body) = body {
                    self.expr(body, tail);
                    let (locals, _stack, refvars) = saved.unwrap();
                    /*if stack < self.ctx.stack {
                        self.ctx
                            .write(Op::Pop(self.ctx.stack as i32 - stack as i32));
                    }*/
                    *self.ctx.locals = locals;
                    *self.refvars = refvars;
                    self.ctx.leave_scope();
                }
            }
            Expr::Block(x) => {
                self.ctx.enter_scope();
                for (i, expr) in x.iter().enumerate() {
                    self.expr(expr, i == x.len() - 1 && tail);
                }
                self.ctx.leave_scope();
            }
            Expr::ArrayInit(exprs) => {
                for x in exprs.iter().skip(1) {
                    self.expr(x, false);
                    self.ctx.write(Op::Push);
                }
                if exprs.len() != 0 {
                    self.expr(&exprs[0], false);
                }
                self.ctx.write(Op::MakeArray(if exprs.len() == 0 {
                    0
                } else {
                    (exprs.len() - 1) as i32
                }));
            }

            Expr::Binop(bop, lhs, rhs) => self.compile_binop(*bop, lhs, rhs),
            Expr::If(cond, then, otherwise) => {
                self.expr(cond, false);
                let jelse = self.ctx.cjmp(false);
                self.expr(then, tail);
                if let Some(otherwise) = otherwise {
                    let jmp = self.ctx.jmp();
                    jelse(self.ctx);
                    self.expr(&otherwise, tail);
                    jmp(self.ctx);
                } else {
                    jelse(self.ctx);
                    self.ctx.write(Op::AccNull);
                }
            }
            Expr::Parenthesis(expr) => self.expr(expr, tail),
            Expr::Function(_, parameters, body) => {
                self.compile_lambda(
                    &parameters
                        .iter()
                        .map(|x| x.clone().into_boxed_str())
                        .collect::<Vec<_>>(),
                    body,
                );
            }
            _ => todo!(),
        }
    }

    pub fn compile_lambda(&mut self, args: &[Box<str>], body: &SpanExpr) {
        let saved = self.refvars.clone();
        for arg in args {
            self.refvars.remove(&**arg);
        }
        self.ctx
            .compile_function(args, |ctx| {
                let mut cc = Compiler {
                    refvars: self.refvars,
                    ctx,
                    toplevel: false,
                };

                cc.expr(body, true);

                Ok(())
            })
            .unwrap();
        *self.refvars = saved;
    }

    pub fn compile_binop(&mut self, bop: TokenKind, e1: &SpanExpr, e2: &SpanExpr) {
        match bop {
            TokenKind::Assign => {
                let a = self.compile_access(e1);
                self.expr(e2, false);
                self.ctx.compile_access_set(&a);
            }
            _ => match (bop, &e1.node, &e2.node) {
                (TokenKind::Eq, _, Expr::Null) => {
                    self.expr(e1, false);
                    self.ctx.write(Op::IsNull);
                }
                (TokenKind::Neq, _, Expr::Null) => {
                    self.expr(e1, false);
                    self.ctx.write(Op::IsNotNull);
                }
                (TokenKind::Eq, Expr::Null, _) => {
                    self.expr(e2, false);
                    self.ctx.write(Op::IsNull);
                }
                (TokenKind::Neq, Expr::Null, _) => {
                    self.expr(e2, false);
                    self.ctx.write(Op::IsNotNull);
                }
                _ => {
                    self.expr(e1, false);
                    self.ctx.write(Op::Push);
                    self.expr(e2, false);
                    use TokenKind::*;
                    let op = match bop {
                        Plus => Op::Add,
                        Minus => Op::Sub,
                        Slash => Op::Div,
                        Star => Op::Mul,
                        Percent => Op::Mod,
                        Less => Op::Lt,
                        LessEq => Op::Lte,
                        Greater => Op::Gt,
                        GreaterEq => Op::Gte,
                        Eq => Op::Eq,
                        Neq => Op::Neq,
                        Shr => Op::Shr,
                        Shl => Op::Shr,
                        UShr => Op::UShr,
                        BitAnd => Op::And,
                        BitOr => Op::Or,
                        Xor => Op::Xor,
                        _ => todo!("{}", bop),
                    };
                    self.ctx.write(op);
                }
            },
        }
    }

    pub fn compile_access(&mut self, expr: &SpanExpr) -> Access {
        match &expr.node {
            Expr::Ident(x) if self.refvars.contains(x) => {
                let acc = self.ctx.access_var(x);
                self.ctx.compile_access_get(&acc);
                self.ctx.write(Op::Push);
                Access::Index(0)
            }
            Expr::Ident(x) => self.ctx.access_var(x),
            Expr::Field(e, f) => {
                self.expr(e, false);
                self.ctx.write(Op::Push);
                let g = self
                    .ctx
                    .global(Rc::new(Global::Symbol(f.clone().into_boxed_str())));
                Access::Field(g as _)
            }
            Expr::Array(arr, ix) => {
                self.expr(ix, false);
                self.ctx.write(Op::Push);
                self.expr(arr, false);
                self.ctx.write(Op::Push);
                Access::Array
            }
            Expr::This => Access::This,
            _ => unreachable!(),
        }
    }
}
