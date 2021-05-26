let fac n =
	cond
	| n = 0 -> 1
	| else -> n * (fac (n - 1)) (* (define (fac n) ( * n (fac (- n 1)))) *)

let pish = 3.1415926535897932384626433832795028841971 (* (define pi 3.1415926535897932384626433832795028841971) *)

type name = {first; last} (* (define-struct name (first last)) *)

3 + 4 * 5 (* (+ 3 ( * 4 5)) *)

;map (fun x -> x * 2) [1; 2; 3] (* (map (lambda (x) ( * x 2)) (list 1 2 3)) *)

let e = 10 in expt e pish; (* (local [(define e 10)] (expt e pi)) *)

cond
| pish = 10 ->
	let msg = "Good approximation"
	in msg
| else -> "Bad approximation"
;

(* (cond [(= pi 10) "Good approximation"]
         [else "Bad approximation"]) *)

;true and false or 2 > 1 (* (or (and true false) (> 2 1)) *)

;pish (* pi *)

;fac 3 (* (fac 3) *)

let succ = add1

;1 + succ 1 + 3
