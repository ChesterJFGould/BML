let fib n =
	cond
	| n = 1 or n = 0 -> 1
	| else -> fib (n - 1) + fib (n - 2)

; fib 5
; 1 + sqr 5 * 9
; add1 3 * 4

type name = {first; last}

let first_name = name-first

let last_name = name-last

; first_name (make-name "Alan" "Turing")
; last_name (make-name "Dennis" "Ritchie")
; (+) 1 2 3 4 5
(* we have to add a space for prefix * as ( immediately followed by would start
 * a comment.
 * this seems not great so I might change the comment delimiter
 *)
 
; foldr ( * ) 1 [1; 2; 3; 4; 5]

let fac n = foldr ( * ) 1 (build-list n add1)
; fac 5

; !true
; true and !false

;
cond
| true -> "True!"
| false -> "False!"
| else -> "Base case!"

; map (fun x -> x * 2) [1; 2; 3]

; foldl (fun x y -> x + y) 0 [1; 2; 3]
; foldl (+) 0 [1; 2; 3]

let at = make-name "Alan" "Turing" in name-first at
