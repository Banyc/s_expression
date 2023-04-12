use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Num(f64),
    Var(String),

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Call {
        func: String,
        params: Vec<Expr>,
    },
    Let {
        name: String,
        rhs: Box<Expr>,
        then: Box<Expr>,
    },
    Fn {
        name: String,
        args: Vec<String>,
        body: Box<Expr>,
        then: Box<Expr>,
    },
}

impl Expr {
    pub fn eval(&self) -> Result<f64, String> {
        let mut ctx = ExprCtx::new();
        self.eval_with_ctx(&mut ctx)
    }

    pub fn eval_with_ctx(&self, ctx: &mut ExprCtx) -> Result<f64, String> {
        match self {
            Expr::Num(n) => Ok(*n),
            Expr::Neg(e) => Ok(-e.eval_with_ctx(ctx)?),
            Expr::Add(e1, e2) => Ok(e1.eval_with_ctx(ctx)? + e2.eval_with_ctx(ctx)?),
            Expr::Sub(e1, e2) => Ok(e1.eval_with_ctx(ctx)? - e2.eval_with_ctx(ctx)?),
            Expr::Mul(e1, e2) => Ok(e1.eval_with_ctx(ctx)? * e2.eval_with_ctx(ctx)?),
            Expr::Div(e1, e2) => Ok(e1.eval_with_ctx(ctx)? / e2.eval_with_ctx(ctx)?),
            Expr::Var(name) => match ctx.var.get(name) {
                Some(val) => Ok(*val),
                None => Err(format!("Variable {} not found", name)),
            },
            Expr::Call { func, params } => {
                let func = match ctx.func.get(func) {
                    Some(func) => func,
                    None => return Err(format!("Function {} not found", func)),
                };

                if func.args.len() != params.len() {
                    return Err(format!(
                        "Function {:?} expects {} arguments, but {} given",
                        func,
                        func.args.len(),
                        params.len()
                    ));
                }

                let mut ctx = ExprCtx {
                    var: ctx.var.clone(),
                    func: ctx.func.clone(),
                };
                let params = params
                    .iter()
                    .map(|p| p.eval_with_ctx(&mut ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                ctx.var.extend(
                    func.args
                        .iter()
                        .cloned()
                        .zip(params)
                        .map(|(arg, param)| (arg, param)),
                );

                func.body.eval_with_ctx(&mut ctx)
            }
            Expr::Let { name, rhs, then } => {
                let val = rhs.eval_with_ctx(ctx)?;
                ctx.var.insert(name.clone(), val);
                then.eval_with_ctx(ctx)
            }
            Expr::Fn {
                name,
                args,
                body,
                then,
            } => {
                let func = Func {
                    args: args.clone(),
                    body: body.as_ref().clone(),
                };
                ctx.func.insert(name.clone(), func);
                then.eval_with_ctx(ctx)
            }
        }
    }
}

pub struct ExprCtx {
    pub var: HashMap<String, f64>,
    pub func: HashMap<String, Func>,
}

impl ExprCtx {
    pub fn new() -> Self {
        Self {
            var: HashMap::new(),
            func: HashMap::new(),
        }
    }
}

impl Default for ExprCtx {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub args: Vec<String>,
    pub body: Expr,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn combine_1() {
        let e = Expr::Let {
            name: "x".to_string(),
            rhs: Expr::Num(5.0).into(),
            then: Expr::Mul(Expr::Var("x".to_string()).into(), Expr::Num(3.0).into()).into(),
        };

        println!("{:?}", e);
    }

    #[test]
    fn eval_num() {
        let e = Expr::Num(5.0);
        assert_eq!(e.eval().unwrap(), 5.0);
    }

    #[test]
    fn eval_neg() {
        let e = Expr::Neg(Expr::Num(5.0).into());
        assert_eq!(e.eval().unwrap(), -5.0);
    }

    #[test]
    fn eval_add() {
        let e = Expr::Add(Expr::Num(5.0).into(), Expr::Num(3.0).into());
        assert_eq!(e.eval().unwrap(), 8.0);
    }

    #[test]
    fn eval_sub() {
        let e = Expr::Sub(Expr::Num(5.0).into(), Expr::Num(3.0).into());
        assert_eq!(e.eval().unwrap(), 2.0);
    }

    #[test]
    fn eval_mul() {
        let e = Expr::Mul(Expr::Num(5.0).into(), Expr::Num(3.0).into());
        assert_eq!(e.eval().unwrap(), 15.0);
    }

    #[test]
    fn eval_div() {
        let e = Expr::Div(Expr::Num(5.0).into(), Expr::Num(3.0).into());
        assert_eq!(e.eval().unwrap(), 5.0 / 3.0);
    }
}
