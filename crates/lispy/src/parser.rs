#![allow(dead_code)]
use crate::ast::{Expr, ExprKind, Import};
use lexpr::{datum::Span, parse::error::Location, Cons, Datum, Value};
use std::marker::PhantomData;

pub struct Parser<'a, R: lexpr::parse::Read<'a>> {
    parser: lexpr::Parser<R>,
    current_pos: Option<Span>,
    marker: PhantomData<&'a R>,
}

#[derive(Debug)]
pub enum ParseError {
    PairExpected,
    SymbolExpected,
    StringExpected,
    LexprError(lexpr::parse::Error),
    Custom(Box<str>),
    Eof,
}
#[derive(Debug)]
pub struct Error {
    pub loc: Option<Location>,
    pub err: ParseError,
}

impl<'a, R: lexpr::parse::Read<'a>> Parser<'a, R> {
    pub fn new(r: R) -> Self {
        Self {
            parser: lexpr::Parser::new(r),
            current_pos: None,
            marker: PhantomData,
        }
    }
    fn datum(&mut self) -> Result<Option<Datum>, Box<str>> {
        self.parser
            .next_datum()
            .map_err(|err| err.to_string().into_boxed_str())
            .map(|datum| {
                datum
                    .as_ref()
                    .map(|datum| self.current_pos = Some(datum.span()));
                datum
            })
    }

    pub fn expect_cons(&mut self) -> Result<(Cons, Span), Box<str>> {
        let datum = self.datum()?;
        if let Some(datum) = datum {
            let span = datum.span();
            match Value::from(datum) {
                Value::Cons(x) => Ok((x, span)),
                x => Err(format!("cons cell expected but `{}` was found", x).into_boxed_str()),
            }
        } else {
            Err("EOF".to_string().into_boxed_str())
        }
    }

    pub fn expect_symbol(&mut self) -> Result<(Box<str>, Span), Box<str>> {
        let datum = self.datum()?;
        if let Some(datum) = datum {
            let span = datum.span();
            match Value::from(datum) {
                Value::Symbol(x) => Ok((x, span)),
                x => Err(format!("symbol expected but `{}` was found", x).into_boxed_str()),
            }
        } else {
            Err("EOF".to_string().into_boxed_str())
        }
    }
    pub fn parse(&mut self) -> Result<Vec<Box<Expr>>, Box<str>> {
        let mut ast = vec![];
        while let Some(datum) = self.datum()? {
            self.current_pos = Some(datum.span());
            ast.push(self.parse_expr(datum.value())?);
        }

        Ok(ast)
    }
    fn parse_array(&mut self, exps: &[lexpr::Value]) -> Result<Vec<Box<Expr>>, Box<str>> {
        exps.iter().map(|exp| self.parse_expr(exp)).collect()
    }

    fn parse_func(
        &mut self,
        first: &lexpr::Value,
        rest: &[lexpr::Value],
    ) -> Result<Expr, Box<str>> {
        let func = self.parse_expr(first)?;
        let args = self.parse_array(rest)?;
        Ok(Expr {
            kind: ExprKind::Call(func, args.into_boxed_slice()),
        })
    }
    fn parse_begin(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        if rest.is_empty() {
            return Err("begin expression has no arguments"
                .to_string()
                .into_boxed_str());
        }
        let exps = self.parse_array(&rest)?;
        Ok(Box::new(Expr {
            kind: ExprKind::Begin(exps.into_boxed_slice()),
        }))
    }

    fn parse_if(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        if rest.len() != 2 || rest.len() != 3 {
            return Err("if expression has incorrect number of arguments"
                .to_string()
                .into_boxed_str());
        }
        let predicate = self.parse_expr(&rest[0])?;
        let consequent = self.parse_expr(&rest[1])?;
        let alternate = if rest.len() == 2 {
            None
        } else {
            Some(self.parse_expr(&rest[2])?)
        };
        Ok(Box::new(Expr {
            kind: ExprKind::If(predicate, consequent, alternate),
        }))
    }
    fn parse_let(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        if rest.len() != 2 && rest.len() != 3 {
            return Err(format!(
                "let expression has incorrect number of arguments. {:?} {}",
                rest,
                rest.len()
            )
            .to_string()
            .into_boxed_str());
        }
        if let Value::Symbol(named) = &rest[0] {
            let bindings = rest[1]
                .to_vec()
                .ok_or_else(|| "Let expression bindings are not in a proper list.")?;
            let bindings_vec: Vec<(Box<str>, Box<Expr>)> = bindings
                .iter()
                .map(|binding| {
                    let binding_vec = binding.to_vec().ok_or_else(|| {
                        "let binding is not a valid list."
                            .to_string()
                            .into_boxed_str()
                    })?;
                    if binding_vec.len() != 2 {
                        return Err("let binding is missing values or contains extra values."
                            .to_string()
                            .into_boxed_str());
                    }
                    let binding_name = binding_vec[0]
                        .as_symbol()
                        .ok_or_else(|| "Let binding does not have a valid name.")?;
                    let binding_val = self.parse_expr(&binding_vec[1])?;
                    Ok((String::from(binding_name).into_boxed_str(), binding_val))
                })
                .collect::<Result<Vec<(Box<str>, Box<Expr>)>, Box<str>>>()?;
            let body = self.parse_begin(&rest[2..])?;
            Ok(Box::new(Expr {
                kind: ExprKind::NamedLet(named.clone(), bindings_vec.into_boxed_slice(), body),
            }))
        } else {
            let bindings = rest[0]
                .to_vec()
                .ok_or_else(|| "Let expression bindings are not in a proper list.")?;
            let bindings_vec: Vec<(Box<str>, Box<Expr>)> = bindings
                .iter()
                .map(|binding| {
                    let binding_vec = binding.to_vec().ok_or_else(|| {
                        "let binding is not a valid list."
                            .to_string()
                            .into_boxed_str()
                    })?;
                    if binding_vec.len() != 2 {
                        return Err("let binding is missing values or contains extra values."
                            .to_string()
                            .into_boxed_str());
                    }
                    let binding_name = binding_vec[0]
                        .as_symbol()
                        .ok_or_else(|| "Let binding does not have a valid name.")?;
                    let binding_val = self.parse_expr(&binding_vec[1])?;
                    Ok((String::from(binding_name).into_boxed_str(), binding_val))
                })
                .collect::<Result<Vec<(Box<str>, Box<Expr>)>, Box<str>>>()?;
            let body = self.parse_begin(&rest[1..])?;
            Ok(Box::new(Expr {
                kind: ExprKind::Let(bindings_vec.into_boxed_slice(), body),
            }))
        }
    }

    fn parse_letrec(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        if rest.len() != 2 && rest.len() != 3 {
            return Err("let expression has incorrect number of arguments."
                .to_string()
                .into_boxed_str());
        }
        if let Value::Symbol(named) = &rest[0] {
            let bindings = rest[1]
                .to_vec()
                .ok_or_else(|| "Let expression bindings are not in a proper list.")?;
            let bindings_vec: Vec<(Box<str>, Box<Expr>)> = bindings
                .iter()
                .map(|binding| {
                    let binding_vec = binding.to_vec().ok_or_else(|| {
                        "let binding is not a valid list."
                            .to_string()
                            .into_boxed_str()
                    })?;
                    if binding_vec.len() != 2 {
                        return Err("let binding is missing values or contains extra values."
                            .to_string()
                            .into_boxed_str());
                    }
                    let binding_name = binding_vec[0]
                        .as_symbol()
                        .ok_or_else(|| "Let binding does not have a valid name.")?;
                    let binding_val = self.parse_expr(&binding_vec[1])?;
                    Ok((String::from(binding_name).into_boxed_str(), binding_val))
                })
                .collect::<Result<Vec<(Box<str>, Box<Expr>)>, Box<str>>>()?;
            let body = self.parse_expr(&rest[2])?;
            Ok(Box::new(Expr {
                kind: ExprKind::NamedLetrec(named.clone(), bindings_vec.into_boxed_slice(), body),
            }))
        } else {
            let bindings = rest[0]
                .to_vec()
                .ok_or_else(|| "Let expression bindings are not in a proper list.")?;
            let bindings_vec: Vec<(Box<str>, Box<Expr>)> = bindings
                .iter()
                .map(|binding| {
                    let binding_vec = binding.to_vec().ok_or_else(|| {
                        "let binding is not a valid list."
                            .to_string()
                            .into_boxed_str()
                    })?;
                    if binding_vec.len() != 2 {
                        return Err("let binding is missing values or contains extra values."
                            .to_string()
                            .into_boxed_str());
                    }
                    let binding_name = binding_vec[0]
                        .as_symbol()
                        .ok_or_else(|| "Let binding does not have a valid name.")?;
                    let binding_val = self.parse_expr(&binding_vec[1])?;
                    Ok((String::from(binding_name).into_boxed_str(), binding_val))
                })
                .collect::<Result<Vec<(Box<str>, Box<Expr>)>, Box<str>>>()?;
            let body = self.parse_expr(&rest[1])?;
            Ok(Box::new(Expr {
                kind: ExprKind::Letrec(bindings_vec.into_boxed_slice(), body),
            }))
        }
    }

    fn parse_args(&mut self, exp: &Value) -> Result<(bool, Box<[Box<str>]>), Box<str>> {
        let mut args = vec![];
        let mut variadic = false;
        if let Some(sym) = exp.as_symbol() {
            variadic = true;
            args.push(sym.to_string().into_boxed_str());
        } else if let Some(list) = exp.as_cons() {
            let mut list = Some(list);
            while let Some(pair) = list {
                let arg = pair.car();
                if let Some(sym) = arg.as_symbol() {
                    args.push(sym.to_string().into_boxed_str());
                } else {
                    return Err(
                        format!("symbol as argument name expected but `{}` found", arg)
                            .into_boxed_str(),
                    );
                }

                let rest = pair.cdr();
                if let Some(sym) = rest.as_symbol() {
                    args.push(sym.to_string().into_boxed_str());
                    variadic = true;
                    break;
                }
                list = pair.cdr().as_cons();
            }
        } else if exp.is_null() {
        } else {
            return Err("lambda expect (<args>) or symbol "
                .to_string()
                .into_boxed_str());
        }

        Ok((variadic, args.into_boxed_slice()))
    }

    fn parse_lambda(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        let (variadic, args) = self.parse_args(&rest[0])?;
        let body = self.parse_begin(&rest[1..])?;

        Ok(Box::new(Expr {
            kind: ExprKind::Fn(None, variadic, args, body),
        }))
    }

    fn parse_field(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        if rest.len() <= 1 {
            return Err(
                format!("field access requires at least 1 field specified").into_boxed_str()
            );
        }

        let mut fields = vec![];
        for i in 0..rest.len() - 1 {
            let field = &rest[i];
            if let Some(sym) = field.as_symbol() {
                fields.push(sym.to_string().into_boxed_str());
            } else {
                return Err(
                    format!("field name must be a symbol but `{}` found", field).into_boxed_str()
                );
            }
        }

        let object = self.parse_expr(&rest[rest.len() - 1])?;
        Ok(Box::new(Expr {
            kind: ExprKind::Field(object, fields.into_boxed_slice()),
        }))
    }

    fn parse_method_call(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        if rest.len() < 2 {
            return Err(
                format!("at least two arguments must be specified for method call")
                    .into_boxed_str(),
            );
        }

        let field = rest[0]
            .as_symbol()
            .ok_or_else(|| format!("Symbol expected").into_boxed_str())?;

        let object = self.parse_expr(&rest[1])?;
        let args = self.parse_array(&rest[2..])?;
        Ok(Box::new(Expr {
            kind: ExprKind::MethodCall(
                field.to_string().into_boxed_str(),
                object,
                args.into_boxed_slice(),
            ),
        }))
    }

    fn parse_ctor(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        if rest.len() < 2 {
            return Err(
                format!("at least two arguments must be specified for method call")
                    .into_boxed_str(),
            );
        }

        let object = self.parse_expr(&rest[0])?;
        let args = self.parse_array(&rest[1..])?;
        Ok(Box::new(Expr {
            kind: ExprKind::Constructor(object, args.into_boxed_slice()),
        }))
    }

    fn parse_while(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        if rest.len() < 2 {
            return Err(format!("(while <cond> <expr>) expected").into_boxed_str());
        }

        let cond = self.parse_expr(&rest[0])?;
        let body = self.parse_begin(&rest[1..])?;
        Ok(Box::new(Expr {
            kind: ExprKind::While(cond, body),
        }))
    }
    fn parse_set(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        if rest.len() != 2 {
            return Err(format!("malformed set!").into_boxed_str());
        }

        let expr = self.parse_expr(&rest[0])?;
        let val = self.parse_expr(&rest[1])?;
        Ok(Box::new(Expr {
            kind: ExprKind::Set(expr, val),
        }))
    }
    fn parse_define(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        if rest.len() == 0 {
            return Err(format!("malformed define").into_boxed_str());
        }
        match &rest[0] {
            Value::Cons(pair) => {
                if rest.len() != 2 {
                    return Err(format!("malformed function definition").into_boxed_str());
                }
                let name = pair
                    .car()
                    .as_symbol()
                    .ok_or_else(|| format!("function name must be a symbol").into_boxed_str())?;
                let (variadic, args) = self.parse_args(&pair.cdr())?;
                let body = self.parse_begin(&rest[1..])?;
                Ok(Box::new(Expr {
                    kind: ExprKind::Fn(
                        Some(name.to_string().into_boxed_str()),
                        variadic,
                        args,
                        body,
                    ),
                }))
            }
            Value::Symbol(name) => {
                if rest.len() > 2 {
                    return Err(format!("malformed define").into_boxed_str());
                }

                let expr = if let Some(expr) = rest.get(1) {
                    Some(self.parse_expr(expr)?)
                } else {
                    None
                };
                Ok(Box::new(Expr {
                    kind: ExprKind::Def(name.clone(), expr),
                }))
            }
            _ => Err(format!(
                "(define (<name> <args>) <body>) or (define <name> <expr?>) expected"
            )
            .into_boxed_str()),
        }
    }

    fn parse_import(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        let mut imports = vec![];
        for path in rest.iter() {
            match path {
                Value::String(str) => imports.push(Import::File(str.to_string())),
                Value::Cons(_) => {
                    if !path.is_list() {
                        return Err(format!(
                            "(import) expects proper list but '{}' was found",
                            path
                        )
                        .into_boxed_str());
                    }

                    let ls = path.to_ref_vec().unwrap();
                    if ls.len() == 0 {
                        return Err(format!("empty import").into_boxed_str());
                    }
                    let mut path = vec![];
                    for p in ls {
                        match p {
                            Value::Symbol(x) => path.push(x.to_string()),
                            _ => {
                                return Err(format!(
                                "(import) expects module name to be a symbol but '{}' was found",
                                p
                            )
                                .into_boxed_str())
                            }
                        }
                    }

                    imports.push(Import::Module(path));
                }
                Value::Symbol(x) => imports.push(Import::Module(vec![x.to_string()])),
                _ => {
                    return Err(
                        format!("(import) expects module path but '{}' was found", path)
                            .into_boxed_str(),
                    )
                }
            }
        }
        Ok(Box::new(Expr {
            kind: ExprKind::Import(imports),
        }))
    }

    fn parse_export(&mut self, rest: &[Value]) -> Result<Box<Expr>, Box<str>> {
        let mut exports = vec![];
        for val in rest.iter() {
            match val {
                Value::Symbol(x) => exports.push(x.to_string()),
                _ => {
                    return Err(format!(
                        "(export) expects symbol as export but '{}' was found",
                        val
                    )
                    .into_boxed_str())
                }
            }
        }
        Ok(Box::new(Expr {
            kind: ExprKind::Export(exports),
        }))
    }
    fn parse_expr(&mut self, value: &Value) -> Result<Box<Expr>, Box<str>> {
        Ok(match value {
            Value::Bool(x) => Box::new(Expr {
                kind: ExprKind::Bool(*x),
            }),
            Value::String(x) => Box::new(Expr {
                kind: ExprKind::Str(x.clone()),
            }),
            Value::Symbol(sym) => Box::new(Expr {
                kind: ExprKind::Var(sym.clone()),
            }),
            Value::Vector(vals) => {
                let mut vals_ = vec![];
                for val in vals.iter() {
                    vals_.push(self.parse_expr(val)?);
                }
                Box::new(Expr {
                    kind: ExprKind::ArrayInit(vals_.into_boxed_slice()),
                })
            }
            Value::Cons(_) => {
                let lst = value.to_vec().ok_or_else(|| {
                    "cons expression is not a valid list"
                        .to_string()
                        .into_boxed_str()
                })?;
                if lst.is_empty() {
                    return Err("Empty list found".to_string().into_boxed_str());
                }
                // We decide how to parse a list based on the first element in the expression;
                // in most cases, just the rest of the vector (i.e. the arguments) will get passed
                // to the individual parsing functions
                let lst_parts = lst.split_at(1);
                let first = &(lst_parts.0)[0];
                let rest = lst_parts.1;
                match first.as_symbol() {
                    Some(val) => match val {
                        "begin" => return self.parse_begin(rest),
                        "import" => return self.parse_import(rest),
                        "export" => return self.parse_export(rest),
                        "let" => return self.parse_let(rest),
                        "letrec" => return self.parse_letrec(rest),
                        "if" => return self.parse_if(rest),
                        "while" => return self.parse_while(rest),
                        "lambda" | "λ" | "𝛌" => return self.parse_lambda(rest),
                        "define" => return self.parse_define(rest),
                        ":" => return self.parse_field(rest),
                        ":-" => return self.parse_method_call(rest),
                        "set!" => return self.parse_set(rest),
                        x if x.starts_with(":-") => {
                            let field = Value::Symbol(x[2..].to_string().into_boxed_str());
                            let mut rest_ = vec![field];
                            rest_.extend_from_slice(rest);
                            return self.parse_method_call(&rest_);
                        }
                        x if x.starts_with(":") => {
                            let field = Value::Symbol(x[1..].to_string().into_boxed_str());
                            let mut rest_ = vec![field];
                            rest_.extend_from_slice(rest);
                            return self.parse_field(&rest_);
                        }
                        "->" => return self.parse_ctor(rest),
                        x if x.starts_with("->") => {
                            let field = Value::Symbol(x[1..].to_string().into_boxed_str());
                            let mut rest_ = vec![field];
                            rest_.extend_from_slice(rest);
                            return self.parse_ctor(&rest_);
                        }

                        _ => (),
                    },

                    None => {}
                }
                let callee = self.parse_expr(first)?;
                let args = self.parse_array(rest)?;
                return Ok(Box::new(Expr {
                    kind: ExprKind::Call(callee, args.into_boxed_slice()),
                }));
            }
            Value::Number(x) => {
                if let Some(x) = x.as_i64() {
                    return Ok(Box::new(Expr {
                        kind: ExprKind::Int(x),
                    }));
                } else if let Some(x) = x.as_f64() {
                    return Ok(Box::new(Expr {
                        kind: ExprKind::Float(x),
                    }));
                } else {
                    todo!()
                }
            }

            _ => todo!(),
        })
    }
}
