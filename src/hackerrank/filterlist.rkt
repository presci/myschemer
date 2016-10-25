#lang racket

(define (upper-limit-2
         ret a rem)
  (cond
    ((null? rem) ret)
    ((> a (car rem)) (upper-limit-2 (cons (car rem) ret) a (cdr rem)))
    (else
     (upper-limit-2 ret a (cdr rem)))))

                       


(define (upper-limit
         a b)
  (upper-limit-2 '() a b))

                       
(upper-limit 3 '(2 3 4 5 5 1 3 4 2))
