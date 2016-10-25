#lang racket


(define (upper-limit a limit rem)
  (cond
    ((null? rem) a)
    ((>  (car rem) limit) (upper-limit a limit (cdr rem)))
    (else
     (upper-limit  (append  a (cons (car rem) '())) limit (cdr rem)))))




(define (f lst)
  (cond
    ((null? lst) '())
    (else
     (upper-limit '() (car lst) (cdr lst)))))

(define (read-list)
  (let ((x (read)))
    (if (eof-object? x)
        (list)
        (cons x (read-list)))))

(let ([lst (read-list)])
  (let ([ans (f lst)])
    (for ([x ans])
      (printf "~a\n" x))))


