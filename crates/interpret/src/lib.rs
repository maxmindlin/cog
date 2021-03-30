mod env;
mod object;

pub use env::Env;
use lexer::TokenKind;
use object::{FuncLiteral, Object, EvalError, BuiltinKind};
use parser::{ExprKind, Identifier, NodeKind, Program, StmtKind, Block};

use std::cell::RefCell;
use std::rc::Rc;

pub(crate) type EnvPointer = Rc<RefCell<Env>>;

// For exiting early if the evaluation of the given
// expr is any variant of the Object::Error.
macro_rules! return_error {
    ($expr:expr) => {
        match &*$expr {
            // TODO this should _not_ be cloning.
            val @ $crate::object::Object::Error(_) => Rc::new(val.clone()),
            val @ _ => Rc::new(val.clone()),
        }
    };
}

pub fn eval(node: NodeKind, env: EnvPointer) -> Rc<Object> {
    use NodeKind::*;
    match node {
        Program(p) => eval_program(p, env),
        Stmt(s) => eval_stmt(&s, env),
        Expr(e) => eval_expr(&e, env),
    }
}

fn eval_program(prgm: Program, env: EnvPointer) -> Rc<Object> {
    let mut res = Rc::new(Object::Null);
    for stmt in prgm.stmts {
        let val = eval(NodeKind::Stmt(stmt), Rc::clone(&env));
        match &*val {
            Object::Return(r) => return Rc::clone(r),
            Object::Error(_) => return val,
            _ => res = val,
        }
    }
    res
}

fn eval_stmt(stmt: &StmtKind, env: EnvPointer) -> Rc<Object> {
    use StmtKind::*;
    match stmt {
        Assign(id, e) => {
            let val = eval_expr(e, Rc::clone(&env));
            match env.borrow_mut().assign(id, val) {
                Ok(_) => Rc::new(Object::Null),
                Err(e) => Rc::new(Object::Error(e)),
            }
        }
        Expr(e) => eval_expr(e, env),
        Return(rv) => match rv {
            None => Rc::new(Object::Null),
            Some(v) => {
                let obj = return_error!(eval_expr(v, Rc::clone(&env)));
                Rc::new(Object::Return(obj))
            }
        },
        Let(id, exp) => {
            match exp {
                None => {
                     match env.borrow_mut().set(id, Rc::new(Object::Null)) {
                        Ok(_) => Rc::new(Object::Null),
                        Err(e) => Rc::new(Object::Error(e)),
                     }
                }
                Some(e) => {
                    let val = eval_expr(e, Rc::clone(&env));
                    match env.borrow_mut().set(id, val) {
                        Ok(_) => Rc::new(Object::Null),
                        Err(e) => Rc::new(Object::Error(e)),
                    }
                }
            }
        },
        While(cond, body) => {
            let mut scope = Env::new();
            scope.add_outer(env);
            let scoped_env = Rc::new(RefCell::new(scope));
            let mut val = eval_expr(cond, Rc::clone(&scoped_env));
            while is_truthy(&*val) {
                let eval = eval_block(body, Rc::clone(&&scoped_env));
                match &*eval {
                    Object::Return(_)
                        | Object::Error(_) => return eval,
                    _ => {
                        val = eval_expr(cond, Rc::clone(&scoped_env));
                    }
                }
            }
            Rc::new(Object::Null)
        }
    }
}

fn eval_expr(expr: &ExprKind, env: EnvPointer) -> Rc<Object> {
    use ExprKind::*;
    match expr {
        Boolean(b) => Rc::new(Object::Boolean(*b)),
        Int(i) => Rc::new(Object::Int(*i)),
        Float(f) => Rc::new(Object::Float(*f)),
        Ident(i) => eval_ident(i, env),
        Str(s) => Rc::new(Object::Str(s.to_owned())),
        Infix(op, lhs, rhs) => {
            let elhs = return_error!(eval_expr(lhs, Rc::clone(&env)));
            let erhs = return_error!(eval_expr(rhs, Rc::clone(&env)));
            Rc::new(eval_infix_expr(op, &elhs, &erhs))
        }
        If(blocks, alt) => {
            let mut scope = Env::new();
            scope.add_outer(env);
            let scoped = Rc::new(RefCell::new(scope));
            for b in blocks.iter() {
                let val = return_error!(eval_expr(&b.cond, Rc::clone(&scoped)));
                if is_truthy(&val) { return eval_block(&b.conseq, Rc::clone(&scoped)); };
            }
            eval_block(alt, scoped)
        }
        Prefix(op, e) => {
            let obj = return_error!(eval_expr(e, Rc::clone(&env)));
            eval_prefix_expr(&op, obj)
        }
        Func(i, block) => Rc::new(Object::Func(Rc::new(FuncLiteral::new(
            i.to_owned(),
            block.to_owned(),
            env,
        )))),
        Call(fexpr, args) => {
            let func = return_error!(eval_expr(&fexpr, Rc::clone(&env)));
            let args = eval_exprs(&*args, env);
            if args.len() == 1 {
                let first = args.first().unwrap();
                if let Object::Error(_) = **first {
                    return Rc::clone(first);
                }
            }
            apply_fn(&func, &args)
        }
        Switch(cond, cases, def) => {
            let mut scope = Env::new();
            scope.add_outer(env);
            let scoped = Rc::new(RefCell::new(scope));
            let val = eval_expr(cond, Rc::clone(&scoped));
            for c in cases.iter() {
                let to_compare = eval_expr(&c.cond, Rc::clone(&scoped));
                if val == to_compare {
                    return eval_block(&c.conseq, Rc::clone(&scoped));
                }
            }
            eval_block(def, scoped)
        }
        Chain(calls) => {
            let mut prev: Option<Rc<Object>> = None;
            for c in calls.iter() {
                match c {
                    Call(fexpr, args) => {
                        let func = return_error!(eval_expr(&fexpr, Rc::clone(&env)));
                        let mut args = eval_exprs(&*args, Rc::clone(&env));
                        if args.len() == 1 {
                            let first = args.first().unwrap();
                            if let Object::Error(_) = **first {
                                return Rc::clone(first)
                            }
                        }
                        if let Some(obj) = prev {
                            args.insert(0, obj);
                        }
                        let chain_val = return_error!(apply_fn(&func, &args));
                        prev = Some(chain_val);
                    }
                    _ => return Rc::new(Object::Error(EvalError::UnknownInfixOp)),
                }
            }
            prev.unwrap()
        }
    }
}

fn eval_exprs(exps: &Vec<ExprKind>, env: EnvPointer) -> Vec<Rc<Object>> {
    let mut out = Vec::new();
    for e in exps {
        let eval = eval_expr(e, Rc::clone(&env));
        match *eval {
            Object::Error(_) => return vec![eval],
            _ => out.push(eval),
        }
    }
    out
}

fn apply_fn(func: &Object, args: &Vec<Rc<Object>>) -> Rc<Object> {
    match func {
        Object::Func(f_lit) => {
            let env = extend_fn_env(f_lit, args);
            let eval = eval_block(&f_lit.body, env);
            match &*eval {
                Object::Return(v) => Rc::clone(v),
                _ => eval,
            }
        },
        Object::Builtin(b) => b.apply(args),
        _ => Rc::new(Object::Error(EvalError::NonFunction))
    }
}

fn extend_fn_env(func: &FuncLiteral, args: &Vec<Rc<Object>>) -> EnvPointer {
    let mut to_extend = Env::new();
    to_extend.add_outer(Rc::clone(&func.env));
    for (i, param) in func.params.iter().enumerate() {
        // ignore this result, since the only way this fails is
        // if there is a duplicate ident. Thats impossible, since it
        // can only affect local scope, and we have just created a fresh
        // env with a completely empty local scope.
        // TODO this might cause weird behavior if they provide duplicate params.
        // Ex. fn (x,x) ...
        let _ = to_extend.set(param, Rc::clone(args.get(i).unwrap()));
    }
    Rc::new(RefCell::new(to_extend))
}

fn eval_ident(id: &Identifier, env: EnvPointer) -> Rc<Object> {
    match env.borrow().get(id) {
        Some(obj) => Rc::clone(&obj),
        None => match BuiltinKind::is_from(&id.name) {
            None => Rc::new(Object::Error(EvalError::UnknownIdent)),
            Some(built) => Rc::new(Object::Builtin(built)),
        }
    }
}

fn eval_infix_expr(op: &TokenKind, lhs: &Object, rhs: &Object) -> Object {
    use Object::*;
    match (lhs, rhs, op) {
        (_, _, TokenKind::EQ) => Boolean(lhs == rhs),
        (_, _, TokenKind::NEQ) => Boolean(lhs != rhs),
        (_, _, TokenKind::And) => {
            let res = is_truthy(lhs) && is_truthy(rhs);
            Boolean(res)
        },
        (_, _, TokenKind::Or) => {
            let res = is_truthy(lhs) || is_truthy(rhs);
            Boolean(res)
        },
        (Str(s), Int(i), TokenKind::Asterisk)
            | (Int(i), Str(s), TokenKind::Asterisk) => Object::Str(s.repeat(*i as usize)),
        (Str(s1), Str(s2), _) => eval_str_infix(op, &s1, &s2),
        (Int(i), Int(j), _) => eval_int_infix(op, *i, *j),
        (Float(i), Float(j), _) => eval_float_infix(op, *i, *j),
        (Float(i), Int(j), _) => eval_float_infix(op, *i, *j as f64),
        (Int(i), Float(j), _) => eval_float_infix(op, *i as f64, *j),
        _ => Error(EvalError::UnknownInfixOp)
    }
}

fn eval_int_infix(op: &TokenKind, i: i64, j: i64) -> Object {
    use Object::*;
    use TokenKind as T;
    match op {
        T::Plus => Int(i + j),
        T::Minus => Int(i - j),
        T::Asterisk => Int(i * j),
        T::Slash => Int(i / j),
        T::LT => Boolean(i < j),
        T::GT => Boolean(i > j),
        T::EQ => Boolean(i == j),
        T::NEQ => Boolean(i != j),
        T::Modulo => Int(i % j),
        _ => Error(EvalError::UnknownInfixOp)
    }
}

fn eval_float_infix(op: &TokenKind, i: f64, j: f64) -> Object {
    use Object::*;
    use TokenKind as T;
    match op {
        T::Plus => Float(i + j),
        T::Minus => Float(i - j),
        T::Asterisk => Float(i * j),
        T::Slash => Float(i / j),
        T::LT => Boolean(i < j),
        T::GT => Boolean(i > j),
        T::EQ => Boolean(i == j),
        T::NEQ => Boolean(i != j),
        T::Modulo => Float(i % j),
        _ => Error(EvalError::UnknownInfixOp)
    }
}

fn eval_str_infix(op: &TokenKind, s1: &str, s2: &str) -> Object {
    use Object::*;
    use TokenKind as T;
    match op {
        T::Plus => Str(s1.to_string() + s2),
        _ => Error(EvalError::UnknownInfixOp)
    }
}

fn eval_prefix_expr(op: &TokenKind, rhs: Rc<Object>) -> Rc<Object> {
    match op {
        TokenKind::Bang => eval_bang_op(rhs),
        TokenKind::Minus => eval_minus_op(rhs),
        _ => Rc::new(Object::Error(EvalError::UnknownPrefixOp)),
    }
}

fn eval_bang_op(rhs: Rc<Object>) -> Rc<Object> {
    use Object::*;
    match *rhs {
        Boolean(b) => Rc::new(Boolean(!b)),
        Null => Rc::new(Boolean(true)),
        _ => Rc::new(Boolean(false)),
    }
}

fn eval_minus_op(rhs: Rc<Object>) -> Rc<Object> {
    use Object::*;
    match *rhs {
        Int(i) => Rc::new(Int(-i)),
        _ => Rc::new(Error(EvalError::UnknownPrefixOp)),
    }
}

fn eval_block(block: &Block, env: EnvPointer) -> Rc<Object> {
    let mut res = Rc::new(Object::Null);
    for s in &block.stmts {
        let val = eval_stmt(s, Rc::clone(&env));
        match *val {
            Object::Error(_)
                | Object::Return(_) => return val,
            _ => res = val,
        }
    }
    res
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(b) => *b,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::*;
    use parser::*;
    use test_case::test_case;

    fn get_eval_output(input: &str) -> Rc<Object> {
        let l = Lexer::new(input);
        let p = Parser::new(l).parse_program().expect("invalid test input");
        let env = Env::new();
        eval(NodeKind::Program(p), Rc::new(RefCell::new(env)))
    }

    #[test_case("let x = 4;", Object::Null; "int assign")]
    #[test_case("let x;", Object ::Null; "null assign")]
    #[test_case("let x = 4; x;", Object::Int(4); "int assign fetch")]
    #[test_case("let x; x;", Object::Null; "null assign fetch")]
    #[test_case("let x; let x;", Object::Error(EvalError::DuplicateDeclare); "IdentExists error")]
    fn test_let_stmt(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input), exp);
    }

    #[test_case("let x = 4; -x;", Object::Int(-4); "neg int")]
    #[test_case("!4", Object::Boolean(false); "bang int")]
    #[test_case("!true", Object::Boolean(false); "bang bool")]
    fn test_prefix(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input), exp);
    }

    #[test_case("let x; x = 5; x;", Object::Int(5); "int assign")]
    #[test_case("let x; x = true; x;", Object::Boolean(true); "bool assign")]
    fn test_assign_stmt(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input), exp);
    }

    #[test_case("return 5;", Object::Int(5); "basic return")]
    #[test_case("return 5; 4", Object::Int(5); "early return")]
    fn test_return_stmt(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input), exp);
    }

    #[test_case(
        "let four = fn(x) { -x };four(-4);",
        Object::Int(4);
        "simple fn call"
    )]
    #[test_case("
let neg = fn(x) {
    fn() { -x };
};
let negTwo = neg(2);
negTwo();",
        Object::Int(-2);
        "simple closure call"
    )]
    fn test_closure_eval(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input), exp);
    }

    #[test_case("3 + 2;", Object::Int(5); "basic add")]
    #[test_case("6 / 2;", Object::Int(3); "whole divide")]
    #[test_case("5 > 3;", Object::Boolean(true); "int GT int")]
    #[test_case("true == false", Object::Boolean(false); "bool EQ bool")]
    #[test_case("4 == false", Object::Boolean(false); "int EQ bool")]
    #[test_case("4 * true", Object::Error(EvalError::UnknownInfixOp); "int MUL bool")]
    #[test_case("4 & 3", Object::Boolean(true); "int AND int")]
    #[test_case("4 & false", Object::Boolean(false); "int AND bool")]
    #[test_case("4 | false", Object::Boolean(true); "int OR bool")]
    #[test_case(r#""foo" + "bar""#, Object::Str("foobar".to_owned()); "str PLUS str")]
    #[test_case("5 % 2", Object::Int(1); "int MODULO int")]
    fn test_infix_eval(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input), exp);
    }

    #[test_case("if (true) { 1 } else { 2 };", Object::Int(1); "basic if")]
    #[test_case("if (2 + 2 > 3) { 1 } else { 2 };", Object::Int(1); "expr cond")]
    #[test_case("if (false) { 1 } else { 2 };", Object::Int(2); "alt eval")]
    #[test_case("if (true) { };", Object::Null; "empty conseq")]
    #[test_case("if (false) { 1 } elif (true) { 2 };", Object::Int(2); "elif eval")]
    fn test_if_eval(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input), exp);
    }

    #[test_case("
switch (1 + 2) {
    (true) => { 0 }
    (3) => { 1 }
};",
        Object::Int(1);
        "basic switch"
    )]
    #[test_case("
switch (1 + 2) {
    (true) => { 0 }
    (5) => { 1 }
};",
        Object::Null;
        "no default is null"
    )]
    #[test_case("
switch (1 + 2) {
    (true) => { 0 }
    (5) => { 1 }
    _ => { 2 }
};",
        Object::Int(2);
        "basic switch with default"
    )]
    #[test_case("
let x = switch (1 + 2) {
    (true) => { 0 }
    (3) => { 1 }
};
x;",
        Object::Int(1);
        "assign switch expr"
    )]
    fn test_switch_eval(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input), exp);
    }

    #[test_case("
let a = fn(x) { x + 1 };
let b = fn(x,y) { x + y };
let c = fn(x) { x + 3 };
a(1) |> b(2) |> c();",
        Object::Int(7);
        "basic fn chain"
    )]
    #[test_case("
let a = fn(x) { x + 1 };
let b = fn(x,y) { x + y };
let c = fn(x) { x + 3 };
let z = a(1)
    |> b(2)
    |> c();
z + 2;",
        Object::Int(9);
        "assign fn chain"
    )]
    fn test_chain_eval(input: &str, exp: Object) {
        assert_eq!(*get_eval_output(input), exp);
    }
}
