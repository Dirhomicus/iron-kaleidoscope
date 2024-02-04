#[derive(Debug, PartialEq)]
pub enum Token {
    Def,
    Extern,
    Identifier(String),
    Number(f64),
    Char(char),
    EOF,
    If,
    For,
    Then,
    Else,
    In,
}

pub struct Lexer<I> where I: Iterator<Item = char> {
    input: I,
    current_char: Option<char>
}

impl<I> Lexer<I> where I: Iterator<Item = char> {
    pub fn new(mut input: I) -> Self {
        let current_char = input.next();
        Lexer { input, current_char }
    }

    fn next_char(&mut self) -> Option<char> {
        self.current_char = self.input.next();
        self.current_char
    }

    pub fn get_token(&mut self) -> Token {
        while matches!(self.current_char, Some(c) if c.is_whitespace()) { self.next_char(); }
        let c = if let Some(ch) = self.current_char { ch } else { return Token::EOF };
        if c == '#' {
            loop {
                match self.next_char() {
                    Some(c) if c == '\r' || c == '\n' => return self.get_token(),
                    None => return Token::EOF,
                    _ => {}
                }
            }
        } 
        if c.is_alphabetic() {
            let mut identifier = String::new();
            while let Some(c) = self.current_char {
                if c.is_alphanumeric() {
                    identifier.push(c);
                    self.next_char();
                } else { break; }
            }
            return match identifier.as_str() {
                "def" =>  Token::Def,
                "extern" => Token::Extern,
                "if" => Token::If,
                "for" =>Token::For,
                "then" => Token::Then,
                "else" => Token::Else,
                "in" => Token::In,
                _ =>  Token::Identifier(identifier)
            };
        } 
        if c.is_digit(10) || c == '.' {
            let mut number_str = String::new();
            while let Some(c) = self.current_char {
                if c.is_digit(10) || c == '.' {
                    number_str.push(c);
                    self.next_char();
                } else { break; }
            }
            return match number_str.parse::<f64>() {
                Ok(n) => Token::Number(n),
                Err(_) => {
                    eprintln!("Invalid number: {}", number_str);
                    Token::EOF
                }
            }
        } 
        self.next_char();
        Token::Char(c)
    }
}