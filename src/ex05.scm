(define lucas001
  (lambda (lat a)
    (cond
     ((null? lat) #f)
     ((eq? a (car lat)) #t)
     (else
      (lucas001 (cdr lat) a)))))

(define lucas002
  (lambda (lat a)
    ((letrec
	 (( di (lambda (lat)
		 (display lat))))
       di) (cdr lat))))

(define lucas003
  (lambda (lat a)
    ((letrec
	 ((di (lambda(lat g)
		(cond
		 ((null? lat) g)
		 ((eq? (car lat) a) (di (cdr lat) (+ g 1)))
		 (else
		  (di (cdr lat) g))))))
       di) lat 0)))
		  
(define member?
  (lambda (lat a)
    ((letrec
	 ((dup? (lambda (lat)
		  (cond
		   ((null? lat) #f)
		   ((eq? (car lat) a) #t)
		   (else
		    (dup? (cdr lat)))))))
       dup?) lat)))








	   
