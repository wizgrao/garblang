use crate::Value::Integer;
use chumsky::prelude::*;
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::io::Write;
use std::{collections::HashMap, error, fmt, io, path::Path, rc::Rc};
use text::TextParser;
use std::env;

pub trait Builtin: Debug {
    fn exec(&self, exp: &Expression, state: &mut State) -> Value;
}

pub trait Op2: Debug {
    fn op(&self, v1: Value, v2: Value) -> Value;
}

struct BuiltinOp2<O>(O);
struct PartialBuiltinOp2<O>(O, Value);

impl<O: Op2> Debug for BuiltinOp2<O> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}
impl<O: Op2> Debug for PartialBuiltinOp2<O> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<O: Op2 + 'static + Clone> Builtin for BuiltinOp2<O> {
    fn exec(&self, exp: &Expression, state: &mut State) -> Value {
        let val = exp.eval(state);
        Value::Builtin(Rc::new(PartialBuiltinOp2::<O>(self.0.clone(), val)))
    }
}

impl<O: Op2 + 'static> Builtin for PartialBuiltinOp2<O> {
    fn exec(&self, exp: &Expression, state: &mut State) -> Value {
        let val = exp.eval(state);
        self.0.op(self.1.clone(), val)
    }
}
#[derive(Debug, Clone)]
pub struct Add;

impl Op2 for Add {
    fn op(&self, x: Value, y: Value) -> Value {
        match (&x, &y) {
            (Integer(xi), Integer(yi)) => Integer(xi + yi).into(),
            _ => Value::Exception(format!("expected integer inputs, got {x} and {y}")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Sub;

impl Op2 for Sub {
    fn op(&self, x: Value, y: Value) -> Value {
        match (&x, &y) {
            (Integer(xi), Integer(yi)) => Integer(xi - yi).into(),
            _ => Value::Exception(format!("expected integer inputs, got {x} and {y}")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Mul;

impl Op2 for Mul {
    fn op(&self, x: Value, y: Value) -> Value {
        match (&x, &y) {
            (Integer(xi), Integer(yi)) => Integer(xi * yi).into(),
            _ => Value::Exception(format!("expected integer inputs, got {x} and {y}")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Div;

impl Op2 for Div {
    fn op(&self, x: Value, y: Value) -> Value {
        match (&x, &y) {
            (Integer(xi), Integer(yi)) => Integer(xi / yi).into(),
            _ => Value::Exception(format!("expected integer inputs, got {x} and {y}")),
        }
    }
}

#[derive(Debug)]
pub struct If;

impl Builtin for If {
    fn exec(&self, exp: &Expression, state: &mut State) -> Value {
        let val = exp.eval(state);
        match val {
            Integer(n) if n <= 0 => Value::Builtin(Rc::new(IfCond(false))),
            Integer(_) => Value::Builtin(Rc::new(IfCond(true))),
            Value::Exception(_) => Value::Builtin(Rc::new(IfCond(false))),
            z => Value::Exception(format!("if condition expected integer, got {:?}", z)),
        }
    }
}

#[derive(Debug)]
pub struct IfCond(bool);
impl Builtin for IfCond {
    fn exec(&self, exp: &Expression, _state: &mut State) -> Value {
        Value::Builtin(Rc::new(IfTrue(self.0, Rc::new(exp.clone()))))
    }
}

#[derive(Debug)]
pub struct IfTrue(bool, Rc<Expression>);
impl Builtin for IfTrue {
    fn exec(&self, exp: &Expression, state: &mut State) -> Value {
        if self.0 {
            self.1.eval(state)
        } else {
            exp.eval(state)
        }
    }
}

#[derive(Debug)]
pub struct IsException;
impl Builtin for IsException {
    fn exec(&self, exp: &Expression, state: &mut State) -> Value {
        let val = exp.eval(state);
        if let Value::Exception(_) = val {
            return Integer(1);
        }
        Integer(0)
    }
}

#[derive(Debug)]
pub struct Prn;
impl Builtin for Prn {
    fn exec(&self, exp: &Expression, state: &mut State) -> Value {
        let val = exp.eval(state);
        println!("{}", val);
        val
    }
}

#[derive(Debug, Clone)]
pub enum Program {
    Expression(Box<Program>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Call(Rc<Expression>, Rc<Expression>),
    Ident(String),
    Integer(i64),
    Definition(String, Rc<Expression>, Rc<Expression>),
    Fn(String, Rc<Expression>),
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Fn(String, Rc<Expression>, Rc<RefCell<State>>),
    Exception(String),
    Builtin(Rc<dyn Builtin>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "integer: {}", i),
            Value::Fn(var_name, _, _) => write!(f, "function {} -> ...", var_name),
            Value::Exception(e) => write!(f, "exception {}", e),
            Value::Builtin(_) => write!(f, "builtin"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct State {
    pub vals: HashMap<String, Value>,
}

impl State {
    pub fn new() -> Self {
        Default::default()
    }
}

impl Default for State {
    fn default() -> Self {
        Self {
            vals: HashMap::from([
                ("add".to_string(), Value::Builtin(Rc::new(BuiltinOp2(Add)))),
                ("sub".to_string(), Value::Builtin(Rc::new(BuiltinOp2(Sub)))),
                ("mul".to_string(), Value::Builtin(Rc::new(BuiltinOp2(Mul)))),
                ("div".to_string(), Value::Builtin(Rc::new(BuiltinOp2(Div)))),
                ("if".to_string(), Value::Builtin(Rc::new(If))),
                (
                    "is_exception".to_string(),
                    Value::Builtin(Rc::new(IsException)),
                ),
                ("prn".to_string(), Value::Builtin(Rc::new(Prn))),
            ]),
        }
    }
}

impl Expression {
    pub fn eval(&self, state: &mut State) -> Value {
        match self {
            Expression::Ident(name) => match state.vals.get(name) {
                Some(x) => x.clone(),
                None => Value::Exception(format!("{} undefined", &name)),
            },
            Expression::Call(fn_expression, arg_expression) => {
                let fn_value = fn_expression.eval(state);
                match fn_value {
                    Value::Fn(var_name, body, closure_state) => {
                        let arg_value = arg_expression.eval(state);
                        let mut cloned_closure = closure_state.borrow().clone();
                        cloned_closure
                            .vals
                            .insert(var_name.clone(), arg_value.clone());
                        body.eval(&mut cloned_closure)
                    }
                    Value::Integer(x) => {
                        Value::Exception(format!("{} is not a function, (eval {:?})", x, self))
                    }
                    Value::Builtin(b) => b.exec(arg_expression, state),
                    exception => exception,
                }
            }
            Expression::Integer(x) => Value::Integer(*x),
            Expression::Definition(var_name, assig_expression, then_expression) => {
                let mut assig_value = assig_expression.eval(state);
                if let Value::Fn(v, x, ref mut s) = &mut assig_value {
                    let s_clone = s.clone();
                    {
                        s.clone()
                            .borrow_mut()
                            .vals
                            .insert(var_name.clone(), Value::Fn(v.clone(), x.clone(), s_clone));
                    }
                }
                state.vals.insert(var_name.clone(), assig_value);
                let ret = then_expression.eval(state);
                ret
            }
            Expression::Fn(var, expression) => Value::Fn(
                var.clone(),
                expression.clone(),
                Rc::new(RefCell::new(state.clone())),
            ),
        }
    }
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

#[derive(Debug, Clone)]
struct ChumWrapper(Vec<Simple<char>>);

impl fmt::Display for ChumWrapper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid error {:?}", self.0)
    }
}

impl error::Error for ChumWrapper {}
#[derive(Debug, Clone)]
struct GarbError(String);

impl fmt::Display for GarbError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid error {:?}", self.0)
    }
}

impl error::Error for GarbError {}

pub fn read_file(path: &str) -> Result<Expression, Box<dyn error::Error>> {
    let src = std::fs::read_to_string(path)?;
    let obj = lang_parser()
        .parse(src)
        .map_err(|e| Box::new(ChumWrapper(e)))?;
    Ok(obj)
}

pub fn eval_expr(x: Expression) -> Value {
    let mut s = State::new();
    x.eval(&mut s)
}

pub fn initialize_state() -> Result<State, Box<dyn error::Error>> {
    let mut s = State::new();
    let lib_files = vec!["lib.garb"];
    let garb_root = env::var("GARBROOT")?;
    for file in lib_files {
        let lib_path = Path::new(&garb_root).join("lib").join(file);
        read_file(lib_path.to_str().ok_or(Box::new(GarbError("invalid library :((".to_string())))?)?.eval(&mut s);
    }
    Ok(s)
}

pub fn repl(x: Option<Expression>) -> Result<(), Box<dyn error::Error>> {
    let mut s = initialize_state()?;
    if let Some(x) = x {
        let ret = x.eval(&mut s);
        println!("{}", ret);
    }
    loop {
        let mut buffer = String::new();
        print!("garb>> ");
        io::stdout().flush()?;
        let stdin = io::stdin(); // We get `Stdin` here.
        stdin.read_line(&mut buffer)?;
        match lang_parser().parse(buffer) {
            Ok(expr) => {
                let val = expr.eval(&mut s);
                println!("{}", val);
                s.vals.insert("result".to_string(), val);
            }
            Err(f) => {
                println!("{:?}", f)
            }
        }
    }
}
