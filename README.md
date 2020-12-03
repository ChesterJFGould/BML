# Description
A transpiler that converts an ML style syntax into the [Advanced Student Language](https://docs.racket-lang.org/htdp-langs/advanced.html). Built mostly for learning purposes.

# Requirements
## Release
+ A computer running Linux
## Source
+ [OCaml](https://ocaml.org/)
+ [Dune](https://dune.build/)

# Installation
## Release
1. Download and unzip the [latest release](https://github.com/Techser/BML/releases)
2. Run `cd release` to change to the unzipped directory.
3. Run `./convert.sh example.ml example.rkt`. This should produce the file `example.rkt` containing the transpiled version of `example.ml`.
4. Run `./tree.sh example.ml`. This should print out the syntax tree of `example.rkt`.

## Source
1. Run `git clone https://github.com/Techser/BML.git`.
2. Run `dune build` in the cloned directory. This may print out some complaints as we are statically linking the code.
3. Run `./convert.sh example.ml example.rkt`. This should produce the file `example.rkt` containing the transpiled version of `example.ml`.
4. Run `./tree.sh example.ml`. This should print out the syntax tree of `example.rkt`.

# Syntax
The following details the syntax of the language and shows the produced ASL code.
A more formal definition can be found in `syntax.bnf`.
## Function Application
`function arg1 arg2 ...` -> `(function arg1 arg2 ...)`

`factorial 2` -> `(factorial 2)`

## Operators
`1 + 1` -> `(+ 1 1)`

`1 + 2 * 3` -> `(+ 1 (* 2 3))`

`6 / 2 - 1` -> `(- (/ 6 2) 1)`

`add1 3 * 4` -> `(* (add1 3) 4)`

## Lists
`[1; 2; 3]` -> `(list 1 2 3)`
`[1; 2; 3; 4;]` -> `(list 1 2 3 4)`

## Definition
`let x = 5`

`let greet_the_world = "Hello, World! - K&R (1978)"` -> `(define greet_the_world "Hello, World! - K&R (1978)")`

`let double x = x * 2` -> `(define (double x) (* x 2))`

## Local Definitions
`let x = 5 in x * 2` -> `(local [(define x 5)] (* x 2))`

`let double x = x * 2 in double 5` -> `(local [(define (double x) (* x 2))] (double 5))`

## If Expressions
`if sqr 5 = 25 then "True" else "False"` -> `(if (= (sqr 5) 25) "True" "False")`

`if false then "You can't see me!" else "You can see me!"` -> `(if false "You can't see me!" "You can see me!")`

## Cond Expressions
    cond
    | true -> "True!"
    | false -> "False!"
    | else -> "Base case!"`
->

    (cond [true "True!"]
          [false "False!"]
          [else "Base case!"])

## Anonymous Functions
`map (fun x -> x * 2) [1; 2; 3]` -> `(map (lambda (x) (* x 2)) (list 1 2 3))`

`foldl (fun x y -> x + y) 0 [1; 2; 3]` -> `(foldl (lambda (x y) (+ x y)) 0 (list 1 2 3))`

## Infix Operator to Prefix Operator
`foldl (+) 0 [1; 2; 3]` -> `(foldl + 0 (list 1 2 3))`

## Types
`type name = {first; last}` -> `(define-struct name (first last))`

`name-first (make-name "Alan" "Turing")` -> `(name-first (make-name "Alan" "Turing"))`

`name-last (make-name "Brian" "Kernighan")` -> `(name-last (make-name "Brian" "Kernighan"))`

## Top Level Expressions
If we want to run multiple separate expressions at the top level of our file we can do so by separating them with a semicolon.

        1 + 1
        ; sqr 3
        

->

        (+ 1 1)
        
        (sqr 3)

We can put the semicolons wherever we like as long as they are between the expressions.

        1 + 1;
        sqr 3

->

        (+ 1 1)
        
        (sqr 3)
        
Keep in mind this is only for expressions, we can have multiple definitions in a row without a semicolon.

        let x = 5
        let y = 6

->

        (define x 5)
        
        (define y 6)

However we must separate a definition followed by an expression.

        let x = 40
        ; x + 2

->

        (define x 40)
        
        (+ x 40)

If we don't follow these rules the parser will get confused and think the expressions are all part of the same expression.

        let x = 40
        
        x + 2
        
->

        (define x (+ (40 x) 2))
# TODO
+ Refactor code to use exceptions instead of the result type
+ Document code better
+ Add at least some integration tests if not unit tests
