use std::rc::Rc;

use super::ast::*;
use waffle::opcode::*;
use waffle::reflect::*;
pub struct Compiler<'a, 'b> {
    pub ctx: &'b mut Context<'a>,
}

impl<'a, 'b> Compiler<'a, 'b> {
    pub fn compile_expr(&mut self, expr: &Expr, tail: bool) -> Result<(), Box<str>> {
        match &expr.kind {
            ExprKind::Begin(exprs) => {
                for (i, expr) in exprs.iter().enumerate() {
                    self.compile_expr(expr, i == exprs.len() - 1 && tail)?;
                }
                Ok(())
            }
            ExprKind::Let(vars, body) => {
                let locals = self.ctx.locals.clone();
                let stack = self.ctx.stack;
                for (var, expr) in vars.iter() {
                    self.compile_expr(expr, false)?;
                    self.ctx.add_var(var, true);
                }
                self.compile_expr(body, tail)?;
                if stack < self.ctx.stack {
                    self.ctx
                        .write(Op::Pop(self.ctx.stack as i32 - stack as i32));
                }
                *self.ctx.locals = locals;
                Ok(())
            }
            ExprKind::Letrec(vars, body) => {
                let locals = self.ctx.locals.clone();
                let stack = self.ctx.stack;
                for (name, _) in vars.iter() {
                    self.ctx.add_var(name, false);
                }

                for (name, value) in vars.iter() {
                    self.compile_expr(value, false)?;
                    let acc = self.ctx.access_var(name);
                    self.ctx.compile_access_set(&acc);
                }
                self.compile_expr(body, tail)?;
                if stack < self.ctx.stack {
                    self.ctx
                        .write(Op::Pop(self.ctx.stack as i32 - stack as i32));
                }
                *self.ctx.locals = locals;
                Ok(())
            }

            // (let fac ([n 10]) ...) -> ((lambda (n fac)) 10 <lambda>)
            ExprKind::NamedLet(name, vars, body) => {
                let params = vars.iter().map(|x| x.0.clone()).collect::<Vec<Box<str>>>();
                let locals = self.ctx.locals.clone();
                let stack = self.ctx.stack;
                self.ctx.add_var(name, false);
                self.compile_lambda(&params, body)?;
                let acc = self.ctx.access_var(name);
                self.ctx.compile_access_set(&acc);

                for (_, val) in vars.iter() {
                    self.compile_expr(val, false)?;
                    self.ctx.write(Op::Push);
                }

                self.ctx.use_var(name);

                if tail {
                    self.ctx.write(Op::TailCall(
                        self.ctx.stack as i16 - self.ctx.limit as i16,
                        params.len() as _,
                    ))
                } else {
                    self.ctx.write(Op::Call(params.len() as _));
                }
                if stack < self.ctx.stack {
                    self.ctx
                        .write(Op::Pop(self.ctx.stack as i32 - stack as i32));
                }
                *self.ctx.locals = locals;
                Ok(())
            }
            ExprKind::Bool(x) => {
                if *x {
                    self.ctx.write(Op::AccTrue);
                    Ok(())
                } else {
                    self.ctx.write(Op::AccFalse);
                    Ok(())
                }
            }
            ExprKind::Int(x) => {
                let x = *x;
                if x as i32 as i64 == x {
                    self.ctx.write(Op::AccInt(x as _));
                } else {
                    let g = self.ctx.global(Rc::new(Global::Int(x)));
                    self.ctx.write(Op::AccGlobal(g as _));
                }
                Ok(())
            }
            ExprKind::Float(x) => {
                let g = self.ctx.global(Rc::new(Global::Float(x.to_bits())));
                self.ctx.write(Op::AccGlobal(g as _));
                Ok(())
            }
            ExprKind::Var(x) => {
                if BUILTINS_VARS.iter().any(|str| &**str == &**x) {
                    let name = x[1..].to_string().into_boxed_str();
                    let name = self.ctx.global(Rc::new(Global::Symbol(name)));
                    self.ctx.write(Op::AccBuiltin(name as _));
                } else if x.starts_with("$") && x.len() > 1 && BUILTINS_LIST.contains(&&x[1..]) {
                    let name = x[1..].to_string().into_boxed_str();
                    let name = self.ctx.global(Rc::new(Global::Symbol(name)));
                    self.ctx.write(Op::AccBuiltin(name as _));
                } else {
                    self.ctx.use_var(x);
                }
                Ok(())
            }

            ExprKind::Call(callee, args) => {
                match callee.kind {
                    ExprKind::Var(ref name) => match &**name {
                        name if BINOP.contains(&&*name) => return self.compile_binop(name, args),
                        name if SPECIAL_CALLS.contains(&&*name) => {
                            return self.compile_special(name, args)
                        }
                        _ => (),
                    },
                    _ => (),
                }
                for arg in args.iter() {
                    self.compile_expr(arg, false)?;
                    self.ctx.write(Op::Push);
                }
                self.compile_expr(callee, false)?;
                if tail {
                    self.ctx.write(Op::TailCall(
                        self.ctx.stack as i16 - self.ctx.limit as i16,
                        args.len() as _,
                    ))
                } else {
                    self.ctx.write(Op::Call(args.len() as _))
                }
                Ok(())
            }
            ExprKind::Fn(name, _variadic, args, body) => {
                let g = if let Some(name) = name {
                    let g = self.ctx.global(Rc::new(Global::Var(name.clone())));
                    Some(g as i32)
                } else {
                    None
                };
                self.compile_lambda(args, body)?;
                if let Some(g) = g {
                    self.ctx.write(Op::SetGlobal(g));
                }

                Ok(())
            }
            _ => todo!("{:?}", expr),
        }
    }

    pub fn compile_special(&mut self, name: &str, args: &[Box<Expr>]) -> Result<(), Box<str>> {
        match name {
            "array-ref" => {
                if args.len() != 2 {
                    return Err(
                        format!("array and index expected as arguments to `array-ref`")
                            .into_boxed_str(),
                    );
                }
                if let ExprKind::Int(ref ix) = args[1].kind {
                    let ix = *ix;
                    if ix as i32 as u32 as i32 as i64 == ix {
                        self.compile_expr(&args[0], false)?;
                        self.ctx.write(Op::AccIndex(ix as _));
                        return Ok(());
                    }
                }
                self.compile_expr(&args[0], false)?;
                self.ctx.write(Op::Push);
                self.compile_expr(&args[1], false)?;
                self.ctx.write(Op::AccArray);
            }
            "array-set!" => {
                if args.len() != 2 {
                    return Err(format!(
                        "array, index and value expected as arguments to `array-set!`"
                    )
                    .into_boxed_str());
                }
                if let ExprKind::Int(ref ix) = args[1].kind {
                    let ix = *ix;
                    if ix as i32 as u32 as i32 as i64 == ix {
                        self.compile_expr(&args[0], false)?;
                        self.ctx.write(Op::Push);
                        self.compile_expr(&args[2], false)?;
                        self.ctx.write(Op::AccIndex(ix as _));
                        return Ok(());
                    }
                }
                self.compile_expr(&args[0], false)?;
                self.ctx.write(Op::Push);
                self.compile_expr(&args[1], false)?;
                self.ctx.write(Op::Push);
                self.compile_expr(&args[2], false)?;
                self.ctx.write(Op::AccArray);
            }
            _ => todo!(),
        }
        Ok(())
    }
    pub fn compile_binop(&mut self, name: &str, args: &[Box<Expr>]) -> Result<(), Box<str>> {
        match name {
            ">" | "<" | ">=" | "<=" | "eq?" | "neq?" => {
                if args.len() != 0 {
                    return Err(
                        format!("only 2 arguments are supported for `{}` operator", name)
                            .into_boxed_str(),
                    );
                }
                self.compile_expr(&args[0], false)?;
                self.ctx.write(Op::Push);
                self.compile_expr(&args[1], false)?;
                self.write_binop(name);
            }
            _ => {
                if args.len() == 0 {
                    return Err(
                        format!("no arguments provided to binary operator `{}`", name)
                            .into_boxed_str(),
                    );
                }

                self.compile_expr(&args[0], false)?;
                for arg in args.iter().skip(1) {
                    self.ctx.write(Op::Push);
                    self.compile_expr(arg, false)?;
                    self.write_binop(name);
                }
            }
        }
        Ok(())
    }
    pub fn compile_lambda(&mut self, args: &[Box<str>], body: &Expr) -> Result<(), Box<str>> {
        self.ctx
            .compile_function(args, |ctx| {
                let mut cc = Compiler { ctx };
                cc.compile_expr(body, true)?;
                Ok(())
            })
            .map_err(|e| e.into_boxed_str())
    }

    fn write_binop(&mut self, name: &str) {
        match name {
            "+" => self.ctx.write(Op::Add),
            "-" => self.ctx.write(Op::Sub),
            "/" => self.ctx.write(Op::Div),
            "*" => self.ctx.write(Op::Mul),
            "%" => self.ctx.write(Op::Mod),
            ">" => self.ctx.write(Op::Gt),
            "<" => self.ctx.write(Op::Lt),
            ">= " => self.ctx.write(Op::Gte),
            "<=" => self.ctx.write(Op::Lte),
            "eq?" => self.ctx.write(Op::Eq),
            "neq?" => self.ctx.write(Op::Neq),
            ">>" => self.ctx.write(Op::Shr),
            "<<" => self.ctx.write(Op::Shl),
            ">>>" => self.ctx.write(Op::UShr),
            "^" => self.ctx.write(Op::Xor),
            "|" => self.ctx.write(Op::Or),
            "&" => self.ctx.write(Op::And),
            _ => unreachable!(),
        }
    }
}

pub const BUILTINS_VARS: &'static [&'static str] = &["$loader", "$exports"];

pub const BUILTINS_LIST: &'static [&'static str] = &[
    "array", "amake", "asize", "acopy", "fullGC", "minorGC", "throw",
];

pub const SPECIAL_CALLS: &'static [&'static str] =
    &["array-ref", "array-set!", "table-ref", "table-set!"];

pub const BINOP: &'static [&'static str] = &[
    "and", "or", "&", "^", "|", "+", "-", "/", "*", "%", ">", "<", ">=", "<=", "eq?", "neq?", ">>",
    "<<", ">>>",
];
