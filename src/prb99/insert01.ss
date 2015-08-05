(define insertsort*
  (lambda (lat)
    ((letrec ((x0 (lambda (lat x)
		    (cond
		     ((null? lat) (list x))
		     ((<= x (car lat)) (cons x lat))
		     (else
		      (cons (car lat) (x0 (cdr lat) x))))))
	      (step0 (lambda (lat)
		       (cond
			((null? lat) '())
			(else
			 (x0 (step0 (cdr lat)) (car lat)))))))
       step0) lat)))
