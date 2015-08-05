(define insertsort* (lambda (lat)
		      ((letrec ((x0 (lambda (lat x)
				      (cond
				       ((null? lat) (list x))
				       ((<= x (car lat)) (cons x lat))
				       (else
					(cons (car lat) (x0 (cdr lat) x))))))
				(a0 (lambda (lat)
				      (cond
				       ((null? lat) '())
				       (else
					(x0 (a0 (cdr lat)) (car lat)))))))
			 a0) lat)))
