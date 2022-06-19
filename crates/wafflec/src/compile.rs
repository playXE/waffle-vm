use std::ops::Range;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use std::str::FromStr;

use ariadne::Report;
use ariadne::ReportBuilder;
use clap::Parser;
use waffle::opcode::*;
use waffle::reflect::*;

use crate::ast::ClassExpr;
use crate::ast::ClassField;
use crate::ast::Export;
use crate::ast::Expr;
use crate::ast::LetDef;
use crate::ast::Pattern;
use crate::ast::Span;
use crate::ast::Spanned;
use crate::ast::TokenKind;

use crate::parser::SpanExpr;
use crate::parser::SyntaxError;

pub enum CompileError {
    ModuleNotFound(Vec<String>, Span),
    PathNotFound(String, Span),
    FileNotFound(String),
    // either syntax error from importing or other compile error
    ImportFailed(String, Span),
    Syntax(Box<SyntaxError>),
    Custom(String, Span),
}

fn select_file(path: &str, is_str: bool) -> Option<String> {
    if Path::new(path).exists() && is_str {
        return Some(path.to_string());
    }
    let waffle = format!("{}.waffle", path);
    let wml = format!("{}.wml", path);
    if Path::new(&wml).exists() {
        return Some(wml);
    }
    if Path::new(&waffle).exists() {
        return Some(waffle);
    }

    None
}

fn search_for_module(paths: &[String], orig: &str, is_str: bool) -> Option<String> {
    if let Some(path) = select_file(orig, is_str) {
        return Some(path);
    }
    for path in paths.iter() {
        let p = format!("{}/{}", path, orig);
        if let Some(path) = select_file(&p, is_str) {
            return Some(path);
        }
    }
    None
}

impl CompileError {
    pub fn report(&self, filename: &str, file: &str) -> ReportBuilder<(String, Range<usize>)> {
        match self {
            CompileError::ImportFailed(path, span) => {
                Report::build(ariadne::ReportKind::Error, filename, span.start).with_label(
                    ariadne::Label::new((filename.to_string(), (*span).into()))
                        .with_message(format!("import from '{}' failed", path)),
                )
            }
            CompileError::Custom(msg, span) => {
                Report::build(ariadne::ReportKind::Error, filename, span.start).with_label(
                    ariadne::Label::new((filename.to_string(), (*span).into()))
                        .with_message(msg.clone()),
                )
            }
            CompileError::Syntax(err) => err.report(filename.to_string(), file),
            CompileError::ModuleNotFound(module, pos) => {
                fn join(vec: &[impl ToString]) -> String {
                    vec.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(".")
                }
                let path = join(module);

                Report::build(ariadne::ReportKind::Error, filename, pos.start)
                    .with_message(format!("module '{}' not found", path))
            }
            CompileError::FileNotFound(x) => Report::build(ariadne::ReportKind::Error, "", 0)
                .with_message(&format!("File '{}' not found", x)),
            CompileError::PathNotFound(path, pos) => {
                Report::build(ariadne::ReportKind::Error, filename, pos.start)
                    .with_message(format!("module '{}' not found", path))
            }
        }
    }
}

pub type CompileResult<T> = Result<T, CompileError>;

pub struct Compiler<'a, 'b> {
    pub ctx: &'b mut Context<'a>,
    pub toplevel: bool,
    pub opts: Rc<CompileOpts>,
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
                        None => {
                            self.ctx.add_var(name, false);
                        }
                        _ => unreachable!(),
                    }
                }
            }
            Pattern::Ident(name) => {
                self.ctx.add_var(name, false);
            }
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
                            self.ctx.write(Op::AccField(g, 0));
                            self.destructuring_assign(&pat.node);
                        }
                        None => {
                            self.ctx.write(Op::AccStack(0));
                            self.ctx.write(Op::AccField(g, 0));
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

    pub fn import_module(&mut self, span: Span, modules: &Vec<String>) -> CompileResult<()> {
        let path = modules
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("/");

        let file = search_for_module(&self.opts.path, &path, false)
            .ok_or_else(|| CompileError::ModuleNotFound(modules.clone(), span))?;
        let mut paths = self.opts.path.clone();
        if let Some(p) = Path::new(&file).parent() {
            paths.push(p.display().to_string());
        }
        if file.ends_with(".wml") {
            let opts = CompileOpts {
                input: PathBuf::from_str(&file).unwrap(),
                output: None,
                path: paths,
                disassembly: self.opts.disassembly,
            };
            let mut src = String::new();
            match compile(opts, &mut src) {
                Ok(_) => (),
                Err(e) => {
                    e.report(&file, &src)
                        .finish()
                        .eprint(ariadne::sources(vec![(file.clone(), src)]))
                        .unwrap();
                    return Err(CompileError::ImportFailed(file, span));
                }
            }
        }

        let out = output_from_input(&Path::new(&file)).unwrap();
        self.loadmodule(&out.display().to_string());
        Ok(())
    }

    pub fn import_str(&mut self, span: Span, path: &str) -> CompileResult<()> {
        let file = search_for_module(&self.opts.path, &path, true)
            .ok_or_else(|| CompileError::PathNotFound(path.to_string(), span))?;

        if file.ends_with(".wml") {
            let mut paths = self.opts.path.clone();
            if let Some(p) = Path::new(&file).parent() {
                paths.push(p.display().to_string());
            }
            let opts = CompileOpts {
                input: PathBuf::from_str(&file).unwrap(),
                output: None,
                path: paths,
                disassembly: self.opts.disassembly,
            };
            let mut src = String::new();
            match compile(opts, &mut src) {
                Ok(_) => (),
                Err(e) => {
                    e.report(&file, &src)
                        .finish()
                        .eprint(ariadne::sources(vec![(file.clone(), src)]))
                        .unwrap();
                    return Err(CompileError::ImportFailed(file, span));
                }
            }
        }

        let out = output_from_input(&Path::new(&file)).unwrap();
        self.loadmodule(&out.display().to_string());
        Ok(())
    }

    fn loadmodule(&mut self, name: &str) {
        let loader = self.ctx.global(Rc::new(Global::Symbol(
            "loader".to_string().into_boxed_str(),
        )));
        let loadmodule = self.ctx.global(Rc::new(Global::Symbol(
            "loadmodule".to_string().into_boxed_str(),
        )));

        let file = self
            .ctx
            .global(Rc::new(Global::Str(name.to_string().into_boxed_str())));
        self.ctx.write(Op::AccGlobal(file as _));
        self.ctx.write(Op::Push);
        self.ctx.write(Op::AccBuiltin(loader as _));
        self.ctx.write(Op::Push);
        self.ctx.write(Op::AccBuiltin(loader as _));
        self.ctx.write(Op::Push);
        self.ctx.write(Op::AccField(loadmodule as _, 0));
        self.ctx.write(Op::ObjCall(2));
    }

    pub fn expr(&mut self, expr: &SpanExpr, tail: bool) -> CompileResult<()> {
        let span = expr.span;
        match &expr.node {
            Expr::Class(name, params, body) => self.class(name, params, body)?,
            Expr::Import(modules) => {
                self.import_module(expr.span, modules)?;
            }
            Expr::Export(exports) => {
                let exports_ = self.ctx.global(Rc::new(Global::Symbol(
                    "exports".to_string().into_boxed_str(),
                )));

                for export in exports.iter() {
                    self.ctx.write(Op::AccBuiltin(exports_ as _));
                    self.ctx.write(Op::Push);
                    match export {
                        Export::Alias(ident, export) => {
                            self.ctx.use_var(ident);
                            let field = self.ctx.global(Rc::new(Global::Symbol(
                                export.to_string().into_boxed_str(),
                            )));
                            self.ctx.write(Op::SetField(field as _, 0));
                        }
                        Export::Identifier(ident) => {
                            self.ctx.use_var(ident);
                            let field = self.ctx.global(Rc::new(Global::Symbol(
                                ident.to_string().into_boxed_str(),
                            )));
                            self.ctx.write(Op::SetField(field as _, 0));
                        }
                    }
                }
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
                self.expr(obj, false)?;
                let g = self
                    .ctx
                    .global(Rc::new(Global::Symbol(field.clone().into_boxed_str())));
                self.ctx.write(Op::AccField(g as _, 0));
            }
            Expr::Apply(callee, args) => self.call(callee, args, tail)?,
            Expr::Call(callee, args) => self.call(callee, args, tail)?,
            Expr::Return(expr) => {
                match expr {
                    Some(x) => self.expr(x, true)?,
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
                self.ctx.write(Op::New(0, 0));
                if !fields.is_empty() {
                    self.ctx.write(Op::Push);
                    for (field, expr) in fields.iter() {
                        self.ctx.write(Op::Push);
                        self.expr(expr, false)?;
                        let g = self
                            .ctx
                            .global(Rc::new(Global::Symbol(field.clone().into_boxed_str())));
                        self.ctx.write(Op::SetField(g as _, 0));
                        self.ctx.write(Op::AccStack(0));
                    }
                    self.ctx.write(Op::Pop(1));
                }
            }
            Expr::Let(defs, body, recursive) => {
                let saved = if body.is_some() {
                    let stack = self.ctx.stack;

                    self.ctx.enter_scope();
                    Some(stack)
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
                                self.expr(value, false)?;
                                self.destructuring_assign(&name.node);
                            }
                            LetDef::Function(name, parameters, body) => {
                                self.compile_lambda(
                                    &parameters
                                        .iter()
                                        .map(|x| x.clone().into_boxed_str())
                                        .collect::<Vec<_>>(),
                                    body,
                                )?;

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
                                self.expr(value, false)?;
                                self.destructuring_assign(&name.node);
                            }
                            LetDef::Function(name, parameters, body) => {
                                self.compile_lambda(
                                    &parameters
                                        .iter()
                                        .map(|x| x.clone().into_boxed_str())
                                        .collect::<Vec<_>>(),
                                    body,
                                )?;
                                self.ctx.add_var(name, true);
                            }
                        }
                    }
                }

                if let Some(body) = body {
                    self.expr(body, tail)?;
                    let _stack = saved.unwrap();
                    /*if stack < self.ctx.stack {
                        self.ctx
                            .write(Op::Pop(self.ctx.stack as i32 - stack as i32));
                    }*/

                    self.ctx.leave_scope();
                }
            }
            Expr::Block(x) => {
                self.ctx.enter_scope();
                for (i, expr) in x.iter().enumerate() {
                    self.expr(expr, i == x.len() - 1 && tail)?;
                }
                self.ctx.leave_scope();
            }
            Expr::ArrayInit(exprs) => {
                for x in exprs.iter().skip(1) {
                    self.expr(x, false)?;
                    self.ctx.write(Op::Push);
                }
                if exprs.len() != 0 {
                    self.expr(&exprs[0], false)?;
                }
                self.ctx.write(Op::MakeArray(if exprs.len() == 0 {
                    0
                } else {
                    (exprs.len() - 1) as i32
                }));
            }

            Expr::Binop(bop, lhs, rhs) => self.compile_binop(*bop, lhs, rhs)?,
            Expr::If(cond, then, otherwise) => {
                self.expr(cond, false)?;
                let jelse = self.ctx.cjmp(false);
                self.expr(then, tail)?;
                if let Some(otherwise) = otherwise {
                    let jmp = self.ctx.jmp();
                    jelse(self.ctx);
                    self.expr(&otherwise, tail)?;
                    jmp(self.ctx);
                } else {
                    jelse(self.ctx);
                    self.ctx.write(Op::AccNull);
                }
            }
            Expr::Parenthesis(expr) => self.expr(expr, tail)?,
            Expr::Function(_, parameters, body) => {
                self.compile_lambda(
                    &parameters
                        .iter()
                        .map(|x| x.clone().into_boxed_str())
                        .collect::<Vec<_>>(),
                    body,
                )?;
            }
            Expr::Array(arr, ix) => {
                self.expr(arr, false)?;
                self.ctx.write(Op::Push);
                self.expr(ix, false)?;
                self.ctx.write(Op::AccArray);
            }
            Expr::While(cond, body, _dowhile) => {
                let start = self.ctx.pos();

                self.expr(cond, false)?;
                let jend = self.ctx.cjmp(false);

                self.expr(body, false)?;
                self.ctx.goto(start as _);
                jend(self.ctx);
            }
            Expr::Try(try_, var, catch) => {
                let trap = self.ctx.trap();
                self.ctx.traps.push(self.ctx.stack);
                self.expr(try_, false)?;
                self.ctx.write(Op::EndTrap);
                self.ctx.traps.pop().unwrap();
                let jend = self.ctx.jmp();
                trap(self.ctx);
                self.ctx.enter_scope();
                self.ctx.add_var(var, true);
                self.expr(catch, tail)?;
                self.ctx.leave_scope();
                jend(self.ctx);
            }
            Expr::Switch(expr, patterns, _) => {
                self.expr(expr, tail)?;
                self.ctx.write(Op::Push);

                let count = patterns
                    .iter()
                    .filter(|(pat, when, _)| matches!(pat.node, Pattern::Default) && when.is_none())
                    .count();
                if count == 0 {
                    return Err(CompileError::Custom(
                        format!("switch statement has no default pattern"),
                        span,
                    ));
                } else if count > 1 {
                    return Err(CompileError::Custom(
                        format!("switch statement has more than one default parameters"),
                        span,
                    ));
                }

                let mut jumps = vec![];
                for (pat, when, then) in patterns.iter() {
                    self.ctx.write(Op::AccStack(0));
                    self.ctx.write(Op::Push);
                    self.pattern(pat)?;
                    if let Some(when) = when {
                        self.ctx.write(Op::Push);
                        self.expr(when, false)?;
                        self.ctx.write(Op::And);
                    }
                    jumps.push((self.ctx.cjmp(true), then));
                }
                let jump_end = self.ctx.jmp();
                let mut njumps = vec![];
                for (j, e) in jumps {
                    j(self.ctx);
                    self.expr(e, tail)?;
                    njumps.push(self.ctx.jmp());
                }

                jump_end(self.ctx);
                for j in njumps {
                    j(self.ctx);
                }
                self.ctx.write(Op::Pop(1));
            }
            Expr::New(callee, args) => self.constructor(callee, args)?,
            _ => todo!(),
        }

        Ok(())
    }

    pub fn pattern(&mut self, pat: &Spanned<Pattern>) -> Result<(), CompileError> {
        match pat.node {
            Pattern::Bool(x) => {
                if x {
                    self.ctx.write(Op::AccTrue)
                } else {
                    self.ctx.write(Op::AccFalse)
                }
                self.ctx.write(Op::Eq);
            }
            Pattern::Int(x) => {
                self.ctx.write(Op::AccInt(x as _));
                self.ctx.write(Op::Eq);
            }
            Pattern::Float(x) => {
                let g = self.ctx.global(Rc::new(Global::Float(x.to_bits())));
                self.ctx.write(Op::AccGlobal(g as _));
                self.ctx.write(Op::Eq);
            }
            Pattern::Str(ref x) => {
                let g = self
                    .ctx
                    .global(Rc::new(Global::Str(x.clone().into_boxed_str())));
                self.ctx.write(Op::AccGlobal(g as _));
                self.ctx.write(Op::Eq);
            }
            Pattern::Symbol(ref x) => {
                let g = self
                    .ctx
                    .global(Rc::new(Global::Symbol(x.clone().into_boxed_str())));
                self.ctx.write(Op::AccGlobal(g as _));
                self.ctx.write(Op::Eq);
            }
            Pattern::Array(ref patterns) => {
                self.ctx.write(Op::AccTrue);
                self.ctx.write(Op::Push);
                for (i, pat) in patterns.iter().enumerate() {
                    self.ctx.write(Op::AccStack(1));
                    self.ctx.write(Op::AccIndex(i as _));
                    self.ctx.write(Op::Push);
                    self.pattern(pat)?;
                    self.ctx.write(Op::And);
                }
            }
            Pattern::Default => {
                self.ctx.write(Op::Pop(1));
                self.ctx.write(Op::AccTrue);
            }
            _ => (),
        }
        Ok(())
    }

    pub fn constructor(&mut self, callee: &SpanExpr, args: &Vec<SpanExpr>) -> CompileResult<()> {
        for arg in args.iter() {
            self.expr(arg, false)?;
            self.ctx.write(Op::Push);
        }
        match callee.node {
            Expr::Field(ref object, ref method) => {
                self.expr(&object, false)?;
                self.ctx.write(Op::Push);
                let g = self
                    .ctx
                    .global(Rc::new(Global::Symbol(method.clone().into_boxed_str())));
                self.ctx.write(Op::AccField(g as _, 0));
                self.ctx.write(Op::ObjCall(args.len() as _));
                return Ok(());
            }
            _ => self.expr(callee, false)?,
        }
        self.ctx.write(Op::New(args.len() as _, 0));
        Ok(())
    }

    pub fn call(
        &mut self,
        callee: &SpanExpr,
        args: &Vec<SpanExpr>,
        tail: bool,
    ) -> CompileResult<()> {
        if let Expr::Builtin(ref x) = callee.node {
            match &**x {
                "$typeof" => {
                    if args.len() != 0 {
                        self.expr(&args[0], false)?;
                    } else {
                        self.ctx.write(Op::AccNull);
                    }
                    self.ctx.write(Op::TypeOf);
                    return Ok(());
                }

                _ => (),
            }
        }
        for arg in args.iter() {
            self.expr(arg, false)?;
            self.ctx.write(Op::Push);
        }
        match callee.node {
            Expr::Field(ref object, ref method) => {
                self.expr(&object, false)?;
                self.ctx.write(Op::Push);
                let g = self
                    .ctx
                    .global(Rc::new(Global::Symbol(method.clone().into_boxed_str())));
                self.ctx.write(Op::AccField(g as _, 0));
                self.ctx.write(Op::ObjCall(args.len() as _));
                return Ok(());
            }
            _ => self.expr(callee, false)?,
        }
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

    pub fn compile_lambda(&mut self, args: &[Box<str>], body: &SpanExpr) -> CompileResult<()> {
        let mut err = None;

        self.ctx
            .compile_function(args, |ctx| {
                let mut cc = Compiler {
                    opts: self.opts.clone(),
                    ctx,
                    toplevel: false,
                };

                match cc.expr(body, true) {
                    Ok(_) => (),
                    Err(e) => err = Some(e),
                }

                Ok(())
            })
            .unwrap();

        if let Some(e) = err {
            Err(e)
        } else {
            Ok(())
        }
    }

    pub fn compile_context<'a, 'b, F: FnOnce(&mut Compiler) -> CompileResult<()>>(
        &mut self,
        args: &[Box<str>],
        closure: F,
    ) -> CompileResult<()> {
        let mut err = None;

        self.ctx
            .compile_function(args, |ctx| {
                let mut cc = Compiler {
                    opts: self.opts.clone(),
                    ctx,
                    toplevel: false,
                };

                match closure(&mut cc) {
                    Ok(_) => (),
                    Err(e) => err = Some(e),
                }
                Ok(())
            })
            .unwrap();

        if let Some(e) = err {
            Err(e)
        } else {
            Ok(())
        }
    }

    pub fn compile_binop(
        &mut self,
        bop: TokenKind,
        e1: &SpanExpr,
        e2: &SpanExpr,
    ) -> CompileResult<()> {
        match bop {
            TokenKind::Assign => {
                let a = self.compile_access(e1)?;
                self.expr(e2, false)?;
                self.ctx.compile_access_set(&a);
            }
            _ => match (bop, &e1.node, &e2.node) {
                (TokenKind::Eq, _, Expr::Null) => {
                    self.expr(e1, false)?;
                    self.ctx.write(Op::IsNull);
                }
                (TokenKind::Neq, _, Expr::Null) => {
                    self.expr(e1, false)?;
                    self.ctx.write(Op::IsNotNull);
                }
                (TokenKind::Eq, Expr::Null, _) => {
                    self.expr(e2, false)?;
                    self.ctx.write(Op::IsNull);
                }
                (TokenKind::Neq, Expr::Null, _) => {
                    self.expr(e2, false)?;
                    self.ctx.write(Op::IsNotNull);
                }
                _ => {
                    self.expr(e1, false)?;
                    self.ctx.write(Op::Push);
                    self.expr(e2, false)?;
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
        Ok(())
    }

    pub fn compile_access(&mut self, expr: &SpanExpr) -> CompileResult<Access> {
        match &expr.node {
            Expr::Ident(x) => Ok(self.ctx.access_var(x)),
            Expr::Field(e, f) => {
                self.expr(e, false)?;
                self.ctx.write(Op::Push);
                let g = self
                    .ctx
                    .global(Rc::new(Global::Symbol(f.clone().into_boxed_str())));
                Ok(Access::Field(g as _))
            }
            Expr::Array(arr, ix) => {
                self.expr(ix, false)?;
                self.ctx.write(Op::Push);
                self.expr(arr, false)?;
                self.ctx.write(Op::Push);
                Ok(Access::Array)
            }
            Expr::This => Ok(Access::This),
            _ => unreachable!(),
        }
    }

    pub fn class(
        &mut self,
        name: &str,
        params: &[Box<str>],
        body: &Spanned<ClassExpr>,
    ) -> CompileResult<()> {
        let g = self.ctx.global(Rc::new(Global::Symbol(
            "object".to_string().into_boxed_str(),
        )));

        self.ctx.write(Op::AccBuiltin(g as _));
        self.ctx.write(Op::New(0, 0));
        let acc = self.ctx.add_var(name, true);

        match body.node {
            ClassExpr::Object(ref body) => {
                self.ctx.enter_scope();
                let mut varset = std::collections::HashMap::new();
                for field in body.iter() {
                    match &field.node {
                        ClassField::Val(name, expr) => {
                            if varset.insert(name.clone(), expr.clone()).is_some() {
                                return Err(CompileError::Custom(
                                    format!("class field '{}' defined twice", name),
                                    field.span,
                                ));
                            }
                            self.ctx.add_class_field(&name);
                        }
                        _ => (),
                    }
                }

                let inherit = body
                    .iter()
                    .find(|x| matches!(x.node, ClassField::Inherit(_, _)));
                match inherit.as_ref().map(|x| &x.node) {
                    Some(ClassField::Inherit(expr, _)) => {
                        self.ctx.compile_access_get(&acc);
                        self.ctx.write(Op::Push);
                        self.expr(expr, false)?;
                        self.ctx.write(Op::Push);

                        let g = self.ctx.global(Rc::new(Global::Symbol(
                            "object".to_string().into_boxed_str(),
                        )));
                        let g2 = self.ctx.global(Rc::new(Global::Symbol(
                            "set_prototype".to_string().into_boxed_str(),
                        )));
                        self.ctx.write(Op::AccBuiltin(g as _));
                        self.ctx.write(Op::Push);
                        self.ctx.write(Op::AccField(g2 as _, 0));
                        self.ctx.write(Op::ObjCall(2));
                    }
                    _ => (),
                }

                for field in body.iter() {
                    match &field.node {
                        ClassField::Method(name, params, body) => {
                            self.ctx.compile_access_get(&acc);
                            self.ctx.write(Op::Push);
                            self.compile_lambda(params, body)?;
                            let g = self
                                .ctx
                                .global(Rc::new(Global::Symbol(name.to_string().into_boxed_str())));
                            self.ctx.write(Op::SetField(g as _, 0));
                        }
                        _ => (),
                    }
                }

                let initializer = body
                    .iter()
                    .find(|x| matches!(x.node, ClassField::Initializer(_)));
                self.ctx.compile_access_get(&acc);
                self.ctx.write(Op::Push);
                self.compile_context(&params, move |cc| {
                    if let Some(inherit) = inherit {
                        match inherit.node {
                            ClassField::Inherit(_, ref args) => {
                                for arg in args {
                                    cc.expr(arg, false)?;
                                    cc.ctx.write(Op::Push);
                                }
                                cc.ctx.write(Op::AccThis);
                                cc.ctx.write(Op::Super(args.len() as _, 0));
                            }
                            _ => unreachable!(),
                        }
                    }
                    for (name, expr) in varset {
                        cc.ctx.write(Op::AccThis);
                        cc.ctx.write(Op::Push);
                        cc.expr(&expr, false)?;
                        let g = cc
                            .ctx
                            .global(Rc::new(Global::Symbol(name.to_string().into_boxed_str())));
                        cc.ctx.write(Op::SetField(g as _, 0));
                    }
                    match initializer.as_ref().map(|x| &x.node) {
                        Some(ClassField::Initializer(expr)) => cc.expr(expr, false),
                        _ => Ok(()),
                    }
                })?;
                let g = self.ctx.global(Rc::new(Global::Symbol(
                    "constructor".to_string().into_boxed_str(),
                )));
                self.ctx.write(Op::SetField(g as _, 0));

                self.ctx.leave_scope();
            }
            _ => todo!(),
        }
        Ok(())
    }
}

pub fn output_from_input(path: &Path) -> Option<PathBuf> {
    let stem = path.file_stem()?;
    let path = if let Some(p) = path.ancestors().nth(1) {
        p.join(stem).with_extension("waffle")
    } else {
        PathBuf::from_str(stem.to_str().unwrap())
            .unwrap()
            .with_extension("waffle")
    };
    Some(PathBuf::from_str(&format!("{}", path.display())).unwrap())
}

pub fn read(opts: &CompileOpts, src: &mut String) -> CompileResult<Vec<SpanExpr>> {
    *src = std::fs::read_to_string(&opts.input)
        .map_err(|_| CompileError::FileNotFound(opts.input.display().to_string()))?;

    let mut p = crate::parser::Parser::new(&src);
    p.parse_program()
        .map_err(|err| CompileError::Syntax(Box::new(err)))
}

pub fn compile(opts: CompileOpts, src: &mut String) -> CompileResult<()> {
    let ast = read(&opts, src)?;
    let opts = Rc::new(opts);
    let mut err = None;
    let (globals, ops) = Context::compile(|ctx| {
        let mut cc = Compiler {
            ctx,
            opts: opts.clone(),
            toplevel: true,
        };

        for expr in ast.iter() {
            println!("cc: {}", expr);
            match cc.expr(expr, false) {
                Ok(_) => (),
                Err(e) => {
                    err = Some(e);
                    return Ok(());
                }
            }
        }
        Ok(())
    })
    .unwrap();

    if let Some(err) = err {
        return Err(err);
    }

    if opts.disassembly {
        let str = waffle::reflect::disassembly(&globals, &ops);
        println!("'{}' disassembly: \n{}", opts.input.display(), str);
    }
    write_code(&globals, &ops, &opts);
    Ok(())
}

pub fn write_code(globals: &[Rc<Global>], ops: &[Op], opts: &CompileOpts) {
    let out = opts
        .output
        .as_ref()
        .map(|x| x.display().to_string())
        .unwrap_or_else(|| {
            output_from_input(&opts.input)
                .expect("no filename")
                .display()
                .to_string()
        });

    let bc = waffle::bytecode::write_module(ops, globals);
    std::fs::write(out, bc).unwrap();
}

#[derive(Parser)]
pub struct CompileOpts {
    pub input: PathBuf,
    output: Option<PathBuf>,
    #[clap(long = "path")]
    pub path: Vec<String>,
    #[clap(long, help = "print bytecode listing")]
    disassembly: bool,
}
