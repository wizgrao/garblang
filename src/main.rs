use garblang::*;
use std::env;
fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() > 1 {
        let program = read_file(&args[1]).unwrap();
        repl(Some(program));
    }
    repl(None);
}
