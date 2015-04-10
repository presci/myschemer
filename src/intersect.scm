(define intersectall-letrec
  (lambda (lset)
    (letrec
	((A (lambda (lset)
	      (cond
	       ((null? (cdr lset)) (car lset))
	       (else
		(intersect (car lset)
			   (A (cdr lset))))))))
      (cond
       ((null? lset) '())
       (else (A lset))))))



