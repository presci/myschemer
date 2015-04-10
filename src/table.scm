(define last
  (lambda (lat)
    ((letrec ((r0 (lambda (lat a)
		  (cond
		   ((null? lat) a)
		   (else
		    (r0 (cdr lat) (car lat)))))))
       r0) lat '())))



(define rowgen-2
  (lambda (lat0 a)
    ((letrec ((r0 (lambda (q count)
		    (cond
		     ((null? lat0) '())
		     ((> count a) (rev-0 q '()))
		     ((>= count lat0) (r0 (cons lat0 q) (+ count 1)))
		     (else
		      (r0 (cons 0 q) (+ count 1))))))
	      (rev-0 (lambda (arg0 arg1)
		       (cond
			((null? arg0) arg1)
			(else
			 (rev-0 (cdr arg0) (cons (car arg0) arg1)))))))
       r0) '() 0)))

    
(define table-gen-0
  (lambda (lat a)
    ((letrec (
	      (k0 (lambda (q)
		    (cond
		     ((null? q) (zero-0 '() a))
		     (else
		      (car q)))))
	      (r0 (lambda (lat q)
		    (cond
		     ((null? lat) q)
		     (else
		      (r0 (cdr lat) (cons (rowgen-2 (car lat) a) q)))))))
       r0) lat '())))

(define zero-0  (lambda (acc a)  (cond  ((<= a 0) acc) (else  (zero-0 (cons 0 acc) (- a 1))))))


(define rowgen-0
  (lambda (lat a)
    ((letrec ((r0 (lambda (lat)
		    (cond
		     ((null? lat) #f)
		     ((eq? (car lat) a) #t)
		     (else
		      (r0 (cdr lat)))))))
       r0) lat)))


(define rowgen-1
  (lambda (lat a)
    (letrec ((r0 (lambda (lat)
		   (cond
		    ((null? lat) #f)
		    ((eq? (car lat) a) #t)
		    (else
		     (r0 (cdr lat)))))))
      (r0 lat))))

(define revs
  (lambda (lat)
    ((letrec ((r0 (lambda (lat d)
		    (cond
		     ((null? lat) d)
		     (else
		      (r0 (cdr lat) (cons (car lat) d)))))))
       r0) lat '())))




(define leaf
  (lambda ()
    ((letrec ((r0 (lambda (a g)
		    (cond
		     ((null? a) (display "none found"))
		     ((= a 0) (+ g 1))
		     (else
		      (+ a 1))))))
       r0) 0 0)))

	

		     
		      
				       
