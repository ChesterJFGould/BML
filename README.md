# Description
A transpiler that converts an ML style syntax into the [Advanced Student Language](https://docs.racket-lang.org/htdp-langs/advanced.html). Built mostly for learning purposes.

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

# TODO
+ Refactor code to use exceptions instead of the result type
+ Document code better
+ Add at least some integration tests if not unit tests
