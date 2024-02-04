use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Number(f64),
    Variable(String),
    Binary(char, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub proto: Prototype,
    pub body: Option<Expr>,
}

pub struct Parser<I>
where
    I: Iterator<Item = char>,
{
    lexer: Lexer<I>,
    current_token: Option<Token>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(lexer: Lexer<I>) -> Self {
        Parser {
            lexer,
            current_token: None,
        }
    }

    pub fn current_token(&self) -> &Token {
        self.current_token
            .as_ref()
            .expect("Expected current token!!!")
    }

    pub fn get_next_token(&mut self) {
        self.current_token = Some(self.lexer.get_token())
    }

    fn parse_number_expr(&mut self) -> Result<Expr, String> {
        match *self.current_token() {
            Token::Number(num) => {
                self.get_next_token();
                Ok(Expr::Number(num))
            }
            _ => Err("this should not happen".into()),
        }
    }

    fn parse_paren_expr(&mut self) -> Result<Expr, String> {
        self.get_next_token(); //Eat '('
        let v = self.parse_expression()?;
        if *self.current_token() == Token::Char(')') {
            self.get_next_token();
            Ok(v)
        } else {
            Err("Expected ')'".into())
        }
    }

    fn parse_identifier_expr(&mut self) -> Result<Expr, String> {
        let name = match self.current_token.take() {
            Some(Token::Identifier(iden)) => {
                self.get_next_token();
                iden
            }
            _ => unreachable!(),
        };

        if *self.current_token() != Token::Char('(') {
            Ok(Expr::Variable(name))
        } else {
            self.get_next_token();
            let mut args = Vec::new();
            if *self.current_token() != Token::Char(')') {
                loop {
                    let arg = self.parse_expression()?;
                    args.push(arg);
                    if *self.current_token() == Token::Char(')') {
                        break;
                    }
                    if *self.current_token() != Token::Char(',') {
                        return Err("Expected ')' or ',' in argument list".into());
                    }
                    self.get_next_token()
                }
            }
            self.get_next_token();
            Ok(Expr::Call(name, args))
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match *self.current_token() {
            Token::Identifier(_) => self.parse_identifier_expr(),
            Token::Number(_) => self.parse_number_expr(),
            Token::Char('(') => self.parse_paren_expr(),
            _ => Err("Unknown token".into()),
        }
    }

    fn parse_expression(&mut self) -> Result<Expr, String> {
        let lhs = self.parse_primary()?;
        self.parse_bin_op_rhs(0, lhs)
    }

    fn parse_bin_op_rhs(&mut self, expr_prec: isize, mut lhs: Expr) -> Result<Expr, String> {
        loop {
            let tok_prec = get_tok_precedence(self.current_token());
            if tok_prec < expr_prec {
                return Ok(lhs);
            }
            let binop = match self.current_token.take() {
                Some(Token::Char(c)) => {
                    self.get_next_token();
                    c
                }
                _ => unreachable!(),
            };
            let mut rhs = self.parse_primary()?;
            let next_prec = get_tok_precedence(self.current_token());
            if tok_prec < next_prec {
                rhs = self.parse_bin_op_rhs(tok_prec + 1, rhs)?
            }
            lhs = Expr::Binary(binop, Box::new(lhs), Box::new(rhs))
        }
    }

    fn parse_prototype(&mut self) -> Result<Prototype, String> {
        let id_name = match self.current_token.take() {
            Some(Token::Identifier(id)) => {
                self.get_next_token();
                id
            }
            other => {
                self.current_token = other;
                return Err("Expected function name in prototype".into());
            }
        };
        if *self.current_token() != Token::Char('(') {
            return Err("Expected '(' in prototype".into());
        }
        let mut args: Vec<String> = Vec::new();
        loop {
            self.get_next_token();

            match self.current_token.take() {
                Some(Token::Identifier(arg)) => args.push(arg),
                Some(Token::Char(',')) => {}
                other => {
                    self.current_token = other;
                    break;
                }
            }
        }
        if *self.current_token() != Token::Char(')') {
            return Err("Expected ')' in prototype".into());
        }
        self.get_next_token(); // Consume ')'.
        Ok(Prototype {
            name: id_name,
            args,
        })
    }

    pub fn parse_definition(&mut self) -> Result<Function, String> {
        self.get_next_token(); // Consume 'def' token.
        let proto = self.parse_prototype()?;
        let expr = self.parse_expression()?;
        Ok(Function { proto, body: Some(expr) })
    }

    pub fn parse_extern(&mut self) -> Result<Prototype, String> {
        self.get_next_token(); // Consume 'extern' token.
        self.parse_prototype()
    }

    pub fn parse_top_level_expr(&mut self) -> Result<Function, String> {
        let e = self.parse_expression()?;
        let proto = Prototype {
            name: "__anon_expr".into(),
            args: Vec::new(),
        };
        Ok(Function { proto, body: Some(e) })
    }
}

fn get_tok_precedence(token: &Token) -> isize {
    match token {
        Token::Char('<') => 10,
        Token::Char('+') | Token::Char('-') => 20,
        Token::Char('*') => 40,
        _ => -1,
    }
}

#[cfg(test)]
mod test {
    use super::{Expr, Function, Parser, Prototype};
    use crate::lexer::Lexer;

    fn parser(input: &str) -> Parser<std::str::Chars> {
        let l = Lexer::new(input.chars());
        let mut p = Parser::new(l);

        // Drop initial coin, initialize cur_tok.
        p.get_next_token();

        p
    }

    #[test]
    fn parse_number() {
        let mut p = parser("13.37");

        assert_eq!(p.parse_number_expr(), Ok(Expr::Number(13.37f64)));
    }

    #[test]
    fn parse_variable() {
        let mut p = parser("foop");

        assert_eq!(p.parse_identifier_expr(), Ok(Expr::Variable("foop".into())));
    }

    #[test]
    fn parse_primary() {
        let mut p = parser("1337 foop \n bla(123) \n if a then b else c \n for x=1,2 in 3");

        assert_eq!(p.parse_primary(), Ok(Expr::Number(1337f64)));

        assert_eq!(p.parse_primary(), Ok(Expr::Variable("foop".into())));

        assert_eq!(
            p.parse_primary(),
            Ok(Expr::Call("bla".into(), vec![Expr::Number(123f64)]))
        );
    }

    #[test]
    fn parse_binary_op() {
        // Operator before RHS has higher precedence, expected AST
        //
        //       -
        //      / \
        //     +     c
        //    / \
        //   a   b
        let mut p = parser("a + b - c");

        let binexpr_ab = Expr::Binary(
            '+',
            Box::new(Expr::Variable("a".into())),
            Box::new(Expr::Variable("b".into())),
        );

        let binexpr_abc = Expr::Binary(
            '-',
            Box::new(binexpr_ab),
            Box::new(Expr::Variable("c".into())),
        );

        assert_eq!(p.parse_expression(), Ok(binexpr_abc));
    }

    #[test]
    fn parse_binary_op2() {
        // Operator after RHS has higher precedence, expected AST
        //
        //       +
        //      / \
        //     a   *
        //        / \
        //       b   c
        let mut p = parser("a + b * c");

        let binexpr_bc = Expr::Binary(
            '*',
            Box::new(Expr::Variable("b".into())),
            Box::new(Expr::Variable("c".into())),
        );

        let binexpr_abc = Expr::Binary(
            '+',
            Box::new(Expr::Variable("a".into())),
            Box::new(binexpr_bc),
        );

        assert_eq!(p.parse_expression(), Ok(binexpr_abc));
    }

    #[test]
    fn parse_prototype() {
        let mut p = parser("foo(a,b)");

        let proto = Prototype {
            name: "foo".into(),
            args: vec!["a".into(), "b".into()],
        };

        assert_eq!(p.parse_prototype(), Ok(proto));
    }

    #[test]
    fn parse_definition() {
        let mut p = parser("def bar( arg0 , arg1 ) arg0 + arg1");

        let proto = Prototype {
            name: "bar".into(),
            args: vec!["arg0".into(), "arg1".into()],
        };

        let body = Expr::Binary(
            '+',
            Box::new(Expr::Variable("arg0".into())),
            Box::new(Expr::Variable("arg1".into())),
        );

        let func = Function { proto, body: Some(body) };

        assert_eq!(p.parse_definition(), Ok(func));
    }

    #[test]
    fn parse_extern() {
        let mut p = parser("extern baz()");

        let proto = Prototype {
            name: "baz".into(),
            args: vec![],
        };

        assert_eq!(p.parse_extern(), Ok(proto));
    }
}
