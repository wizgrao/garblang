# garblang


garblang is a wip programming language implemented in rust. right now its a purely functional integer based language.

example program:
```
succ = fn x {x + 1};
fib n =
    if n
        (fib (n - 1) + fib (n - 2))
        1;
fib 3 * succ(9)
```
This program demonstrates two ways of defining a function, one using an anonymous function and one using a more haskell like syntax.
All functions are curried.

garb can be run as a repl, just run `cargo run`. Alternatively a repl can be run after running a program. The output of the program will be printed along with a repl started with the constants defined above in scope. For example, with `fib.garb` as written above, we can run `cargo run -- fib.garb` and start a repl.
```
cargo run -- fib.garb
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.08s
     Running `target/debug/garblang fib.garb`
integer: 50
garb>> fib 5
integer: 13
```
Here, `garb>>` is the prompt, and the user calls fib defined in the file with input 5. 


I want to make this language lazy, and output sounds. But right now it's a strict calculator.