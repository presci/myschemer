(define foto
  (lambda (lat)
    (display lat)))
(define ribbon04
  (lambda (lat)
    ((letrec
	 ((di (lambda (lat a)
		(display lat))))
       di) (cdr lat) 0)))


(define foto01
  (lambda (lat)
    ((letrec (( di (lambda (lat a)
		     (cond 
		      ((null? lat) a)
		      (else
		       (di (cdr lat) (+ a 1)))))))
	      di) lat 0)))
(define bako01
  (lambda (lat a)
    ((letrec (( di (lambda (lat)
		     (cond
		      ((null? lat) #f)
		      ((eq? a (car lat)) #t)
		      (else
		       (di (cdr lat)))))))
       di) lat)))
		       