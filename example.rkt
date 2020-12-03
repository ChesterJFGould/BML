#reader(lib "htdp-advanced-reader.ss" "lang")
	((modname example2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings 
	 #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (fac n)
        (cond [(= n
                  0) 1]
              [else (* n
                       (fac (- n
                               1)))]))



(define pish
        3.141593)



(define-struct name
               (first last))



(+ 3
   (* 4
      5))



(map (lambda (x)
             (* x
                2))
     (list 1
           2
           3))



(local [(define e
               10)]
       (expt e
             pish))



(cond [(= pish
          10) (local [(define msg
                               "Good approximation")]
                       msg)]
      [else "Bad approximation"])



(or (and true
         false)
    (> 2
       1))



pish



(fac 3)



(define succ
        add1)



(+ (+ 1
      (succ 1))
   3)
