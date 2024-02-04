use std::io::{self, Write};

use iron_kaleidoscope::lexer::Lexer;
use iron_kaleidoscope::parser::Parser;


fn main() {
    mainloop()
}

fn mainloop() {
    println!("Welcome to the Kaleidoscope REPL!");
    println!("Enter expressions to evaluate, or 'exit' to quit.");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let input = input.trim();

        if input.eq_ignore_ascii_case("exit") {
            break;
        }

        let lexer = Lexer::new(input.chars());
        let mut parser = Parser::new(lexer);

        match parser.parse_top_level_expr() {
            Ok(expr) => {
                println!("Parsed expression: {:?}", expr);
                // Evaluate the expression and print the result (you'll need to implement this).
                // For simplicity, we'll just print the parsed expression.
            }
            Err(e) => println!("Error: {}", e),
        }
    }
}