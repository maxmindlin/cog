mod env;
mod object;

use env::Env;
use object::Object;
use parser::{NodeKind, StmtKind, ExprKind, Identifier, Program};

pub fn eval(node: NodeKind, env: &mut Env) -> Object {
    use NodeKind::*;
    match node {
        Program(p) => eval_program(p, env),
        Stmt(s) => eval_stmt(&s, env),
        Expr(e) => eval_expr(&e, env),
    }
}

fn eval_program(prgm: Program, env: &mut Env) -> Object {
    let mut res = Object::Null;
    for stmt in prgm.stmts {
        let val = eval(NodeKind::Stmt(stmt), env);
        match val {
            Object::Error => return val,
            _ => res = val,
        }
    }
    res
}

fn eval_stmt(stmt: &StmtKind, env: &mut Env) -> Object {
    use StmtKind::*;
    match stmt {
        Expr(e) => eval_expr(e, env),
        Return(rv) => match rv {
            None => Object::Null,
            Some(v) => {
                let _val = eval_expr(v, env);
                unimplemented!()
            }
        }
        Let(id, exp) => match exp {
            None => {
                env.set(id, Object::Null);
                Object::Null
            }
            Some(e) => {
                let val = eval_expr(e, env);
                env.set(id, val);
                Object::Null
            }
        }
    }
}

fn eval_expr(expr: &ExprKind, env: &mut Env) -> Object {
    use ExprKind::*;
    match expr {
        Int(i) => Object::Int(*i),
        Ident(i) => eval_ident(i, env),
    }
}

fn eval_ident(id: &Identifier, env: &mut Env) -> Object {
    match env.get(id) {
        Some(obj) => *obj,
        None => Object::Error
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;
    use lexer::*;
    use test_case::test_case;

    fn get_eval_output(input: &str) -> Object {
        let l = Lexer::new(input);
        let p = Parser::new(l).parse_program().expect("invalid test input");
        let mut env = Env::new();
        eval(NodeKind::Program(p), &mut env)
    }

    #[test_case("let x = 4;", Object::Null; "int assign")]
    #[test_case("let x;", Object ::Null; "null assign")]
    #[test_case("let x = 4; x;", Object::Int(4); "int assign fetch")]
    #[test_case("let x; x;", Object::Null; "null assign fetch")]
    fn test_let_stmt(input: &str, exp: Object) {
        assert_eq!(get_eval_output(input), exp);
    }
}
