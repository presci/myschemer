(define multirember-letrec
  (lambda (a lat)
    ((letrec
	 ((mr (lambda (lat)
		(cond
		 ((null? lat) '())
		 ((eq? a (car lat)) (mr (cdr lat)))
		 (else
		  (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))		  


(define rock?
  (lambda (a lat)
    ((letrec 
	 ((di (lambda (lat)
		(cond
		 ((null? lat) '())
		 ((eq? a (car lat)) (di (cdr lat)))
		 (else
		  (cons (car lat) (di (cdr lat))))))))
       di) lat)))




(define ribbon01
  (lambda (lat)
    ((letrec 
	 ((di (lambda (lat)
		(display lat))))
       di) (cdr lat))))

(define ribbon02
  (lambda (lat)
    (letrec (( di (lambda (lat)
		  (display lat))))
    (di (cdr lat)))))



(define ribbon03
  (lambda (lat)
    ((letrec
	 ((di (lambda (lat)
		(display lat)))
	  (ki (lambda (lat)
		(di (cdr lat)))))
       ki) (cdr lat))))

(define ribbon04
  (lambda (lat)
    ((letrec
	 ((di (lambda (lat a)
		(display lat))))
       di) (cdr lat) 0)))

(define ribbon05
  (lambda (lat)
    ((letrec
	 (( di (lambda (lat a)
		 (display lat)))
	  (ki (lambda (lat)
		(di (cdr lat) 0))))
       ki) (cdr lat))))