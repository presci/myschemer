
;;; P09
;;; If a list contains repeated elements they should be placed in separate sublists.
;;; Example:
;;; * (pack '(a a a a b c c a a d e e e e))
;;; ((A A A A) (B) (C C) (A A) (D) (E E E E))

(define pack*
  (lambda (lat)
    (letrec ((r0 (lambda (lat c0 acc pot)
		   (cond
		    ((null? lat) (cons acc pot))
		    ((eq? (car lat) c0)
		     (r0 (cdr lat) c0 (cons (car lat) acc) pot))
		    (else
		     (r0 (cdr lat) (car lat) (cons (car lat) '()) (cons acc pot))))))) (r0 (cdr lat) (car lat) (cons (car lat) '()) '()))))


(define pack0*
  (lambda (lat)
    (letrec
	((r0 (lambda (lat acc pot)
	       (cond
		((null? lat) (cons acc pot))
		((eq? (car lat) (car acc))
		 (r0 (cdr lat) (cons (car lat) acc) pot))
		(else
		 (r0 (cdr lat) (cons (car lat) '()) (cons acc pot)))))))
      (r0 (cdr lat) (cons (car lat) '()) '()))))

					 
										  





;;; divi
(define div*
  (lambda (a b)
    ((letrec ((x0
	       (lambda (x c)
		 (cond
		  ((or ( < x b) (= x 0)) c)
		  (else
		   (x0 (- x b) (+ c 1)))))))
       x0) a 0)))


;;; reverse a list
(define rev0
  (lambda (lat)
    (letrec ((r0 (lambda (lat acc)
		   (cond
		    ((null? lat) acc)
		    (else
		     (r0 (cdr lat) (cons (car lat) acc)))))))
	(r0 lat '()))))

	     
		   
		   
