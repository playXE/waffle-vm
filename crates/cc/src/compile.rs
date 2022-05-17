use std::rc::Rc;

use waffle::opcode::*;
use waffle::reflect::*;

use crate::ast::Expr;
use crate::ast::LetDef;
use crate::parser::SpanExpr;

pub struct Compiler<'a, 'b> {
    pub ctx: &'b mut Context<'a>,
    pub toplevel: bool,
}

impl Compiler<'_, '_> {
    pub fn expr(&mut self, expr: &SpanExpr, tail: bool) {
        match &expr.node {
            Expr::Ident(x) => self.ctx.use_var(x),
            Expr::Builtin(x) => {
                let name = self
                    .ctx
                    .global(Rc::new(Global::Symbol(x.clone().into_boxed_str())));
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
            Expr::Let(defs, body, recursive) => {
                let saved = if body.is_some() {
                    let locals = self.ctx.locals.clone();
                    let stack = self.ctx.stack;
                    Some((locals, stack))
                } else {
                    None
                };

                if *recursive {
                    for def in defs.iter() {
                        match &def.node {
                            LetDef::Variable(name, _) => {
                                self.ctx.add_var(name, false);
                            }
                            LetDef::Function(name, _, _) => {
                                self.ctx.add_var(name, false);
                            }
                        }
                    }

                    for def in defs.iter() {
                        match &def.node {
                            LetDef::Variable(name, value) => {
                                let acc = self.ctx.access_var(name);
                                self.expr(value, false);
                                self.ctx.compile_access_set(&acc);
                            }
                            LetDef::Function(name, parameters, body) => {
                                let acc = self.ctx.access_var(name);
                                self.compile_lambda(
                                    &parameters
                                        .iter()
                                        .map(|x| x.clone().into_boxed_str())
                                        .collect::<Vec<_>>(),
                                    body,
                                );
                                self.ctx.compile_access_set(&acc);
                            }
                        }
                    }
                } else {
                    for def in defs.iter() {
                        match &def.node {
                            LetDef::Variable(name, value) => {
                                self.expr(value, false);
                                self.ctx.add_var(name, true);
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
                    let (locals, stack) = saved.unwrap();
                    if stack < self.ctx.stack {
                        self.ctx
                            .write(Op::Pop(self.ctx.stack as i32 - stack as i32));
                    }
                    *self.ctx.locals = locals;
                }
            }
            Expr::Block(x) => {
                for (i, expr) in x.iter().enumerate() {
                    self.expr(expr, i == x.len() - 1 && tail);
                }
            }

            Expr::Binop(bop, lhs, rhs) => {
                use crate::ast::TokenKind::*;
                self.expr(lhs, false);
                self.ctx.write(Op::Push);
                self.expr(rhs, false);
                match *bop {
                    Plus => self.ctx.write(Op::Add),
                    Minus => self.ctx.write(Op::Sub),
                    Star => self.ctx.write(Op::Mul),
                    Slash => self.ctx.write(Op::Div),
                    Eq => self.ctx.write(Op::Eq),
                    Neq => self.ctx.write(Op::Neq),
                    Less => self.ctx.write(Op::Lt),
                    LessEq => self.ctx.write(Op::Lte),
                    Greater => self.ctx.write(Op::Gt),
                    GreaterEq => self.ctx.write(Op::Gte),
                    _ => todo!(),
                }
            }
            Expr::If(cond, then, otherwise) => {
                self.expr(cond, false);
                let cjmp = self.ctx.cjmp(false);
                self.expr(then, tail);
                if let Some(otherwise) = otherwise {
                    let jmp = self.ctx.jmp();
                    cjmp(self.ctx);
                    self.expr(&otherwise, tail);
                    jmp(self.ctx);
                } else {
                    self.ctx.write(Op::AccNull);
                    cjmp(self.ctx);
                }
            }

            _ => todo!(),
        }
    }

    pub fn compile_lambda(&mut self, args: &[Box<str>], body: &SpanExpr) {
        self.ctx
            .compile_function(args, |ctx| {
                let mut cc = Compiler {
                    ctx,
                    toplevel: false,
                };
                cc.expr(body, true);
                Ok(())
            })
            .unwrap();
    }
}
