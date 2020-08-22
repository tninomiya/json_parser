use lexer::lexer;
use std::io;

fn prompt(s: &str) -> io::Result<()> {
    use std::io::{stdout, Write};
    let stdout = stdout();
    let mut stdout = stdout.lock();
    stdout.write(s.as_bytes())?;
    stdout.flush()
}

fn main() {
    use std::io::{stdin, BufRead, BufReader};
    // let mut interp = Interpreter::new();

    let stdin = stdin();
    let stdin = stdin.lock();
    let stdin = BufReader::new(stdin);
    let mut lines = stdin.lines();

    loop {
        prompt("> ").unwrap();
        // ユーザの入力を取得する
        if let Some(Ok(line)) = lines.next() {
            let token = lexer::lex(&line);
            println!("{:?}", token);
        } else {
            break;
        }
    }
}
