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

Some basic functions and syntax sugar for lists are defined:
```
lost = 4 : 8 : 15 : 16 : 23 : 42 : list_init;
print_all lost
```
this prints
```
integer: 4
integer: 8
integer: 15
integer: 16
integer: 23
integer: 42
integer: 0
```
(0 is the value of the print_all function, the rest are printed by the `prn` function).
garb also supports the haskell-style right associated function call operator `$`.
```
print_all $ map (add 1) $ map (mul 2) lost
```
prints
```
integer: 9
integer: 17
integer: 31
integer: 33
integer: 47
integer: 85
integer: 0
```

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