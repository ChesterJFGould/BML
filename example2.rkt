#reader(lib "htdp-advanced-reader.ss" "lang")
	((modname example2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings 
	 #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (fib n)
        (cond [(or (= n
                      1)
                   (= n
                      0)) 1]
              [else (+ (fib (- n
                               1))
                       (fib (- n
                               2)))]))



(fib 5)



(+ 1
   (* (sqr 5)
      9))



(* (add1 3)
   4)



(define-struct name
               (first last))



(define first_name
        name-first)



(define last_name
        name-last)



(first_name (make-name "Alan"
                       "Turing"))



(last_name (make-name "Dennis"
                      "Ritchie"))



(+ 1
   2
   3
   4
   5)



(foldr *
       1
       (list 1
             2
             3
             4
             5))



(define (fac n)
        (foldr *
               1
               (build-list n
                           add1)))



(fac 5)



(not true)



(and true
     (not false))



(cond [true "True!"]
      [false "False!"]
      [else "Base case!"])



(map (lambda (x)
             (* x
                2))
     (list 1
           2
           3))



(foldl (lambda (x y)
               (+ x
                  y))
       0
       (list 1
             2
             3))



(foldl +
       0
       (list 1
             2
             3))



(local [(define at
               (make-name "Alan"
                          "Turing"))]
       (name-first at))
