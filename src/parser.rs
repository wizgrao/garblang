use std::rc::Rc;
use chumsky::{text, Parser};
use chumsky::prelude::{choice, end, just, recursive, Simple};
use chumsky::text::TextParser;

#[derive(Debug, Clone)]
pub enum Expression {
    Call(Rc<Expression>, Rc<Expression>),
    Ident(String),
    Integer(i64),
    Definition(String, Rc<Expression>, Rc<Expression>),
    Fn(String, Rc<Expression>),
}
pub fn lang_parser() -> impl Parser<char, Expression, Error = Simple<char>> {
    let x = recursive(|expr| {
        let ident = text::ident().padded().map(Expression::Ident);

        let pos_num = text::int(10)
            .map(|x: String| Expression::Integer(x.parse::<i64>().unwrap()))
            .padded();

        let neg_num = just('-')
            .ignore_then(text::int(10))
            .map(|x| Expression::Integer(-x.parse::<i64>().unwrap()))
            .padded();

        let num = pos_num.or(neg_num);

        let bounded_expr = expr.clone().delimited_by(just("("), just(")")).padded();

        let left_fn = ident.or(bounded_expr.clone());

        let atom = bounded_expr.clone().or(num).or(ident);

        let fn_call = left_fn
            .padded()
            .then(atom.clone().repeated())
            .foldl(|x: Expression, y: Expression| Expression::Call(Rc::new(x), Rc::new(y)));

        let fn_def = text::keyword("fn")
            .ignore_then(text::ident().padded())
            .then(expr.clone().padded().delimited_by(just("{"), just("}")))
            .map(|(var, x): (String, Expression)| Expression::Fn(var, Rc::new(x)));

        let let_exp = text::ident()
            .then(text::ident().padded().repeated())
            .then_ignore(just('=').padded())
            .then(expr.clone().padded())
            .map(|((var, args), assign)| (args, (var, assign)))
            .foldr(|arg, (var, assign)| (var, Expression::Fn(arg, assign.into())))
            .then_ignore(just(';').padded())
            .then(expr.clone())
            .map(|((var, assig), then): ((String, Expression), Expression)| {
                Expression::Definition(var, assig.into(), then.into())
            });

        let bruh = choice((fn_def, let_exp, fn_call, atom.clone()));

        let plus = just("+").padded().to((|l: Expression, r: Expression| {
            Expression::Call(
                Expression::Call(Expression::Ident("add".to_string()).into(), l.into()).into(),
                r.into(),
            )
        }) as fn(Expression, Expression) -> Expression);

        let minus = just("-").padded().to((|l: Expression, r: Expression| {
            Expression::Call(
                Expression::Call(Expression::Ident("sub".to_string()).into(), l.into()).into(),
                r.into(),
            )
        }) as fn(Expression, Expression) -> Expression);

        let mul = just("*").padded().to((|l: Expression, r: Expression| {
            Expression::Call(
                Expression::Call(Expression::Ident("mul".to_string()).into(), l.into()).into(),
                r.into(),
            )
        }) as fn(Expression, Expression) -> Expression);

        let div = just("/").padded().to((|l: Expression, r: Expression| {
            Expression::Call(
                Expression::Call(Expression::Ident("div".to_string()).into(), l.into()).into(),
                r.into(),
            )
        }) as fn(Expression, Expression) -> Expression);

        let col = just(":").padded().to((|l: Expression, r: Expression| {
            Expression::Call(
                Expression::Call(Expression::Ident("append".to_string()).into(), l.into()).into(),
                r.into(),
            )
        }) as fn(Expression, Expression) -> Expression);

        let dollar = just("$").padded().to((|l: Expression, r: Expression| {
            Expression::Call(l.into(), r.into())
        }) as fn(Expression, Expression) -> Expression);

        let append = bruh
            .clone()
            .then(col.or(dollar))
            .repeated()
            .then(bruh)
            .foldr(|(expr2, op), expr| op(expr2, expr));

        let mul = append
            .clone()
            .then(mul.or(div).then(append.padded()).repeated())
            .foldl(|expr, (op, expr2)| op(expr, expr2));

        let add = mul
            .clone()
            .then(minus.or(plus).then(mul.padded()).repeated())
            .foldl(|expr, (op, expr2)| op(expr, expr2));

        return add;
    })
        .padded();
    let let_exp = text::ident()
        .then(text::ident().padded().repeated())
        .then_ignore(just('=').padded())
        .then(x.clone().padded())
        .map(|((var, args), assign)| (args, (var, assign)))
        .foldr(|arg, (var, assign)| (var, Expression::Fn(arg, assign.into())))
        .map(|(var, assig): (String, Expression)| {
            Expression::Definition(var.clone(), assig.into(), Expression::Ident(var).into())
        });
    let_exp.then_ignore(end()).or(x.then_ignore(end()))
}
