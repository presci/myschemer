;;
;; ((0 1) (2 5) (3 8)) -> ((0 1) (2 8))
;; ((0 1) (4 5) (3 9)) -> ((0 1) (3 9))
;;


(define min (lambda (a b) (cond ((< a b) a) (else b))))

(define max (lambda (a b) (cond ((> a b)  a) (else  b))))

(define add (lambda (lat acc) (cond ((null? lat) acc) (else (add (cdr lat) (cons (car lat) acc))))))


(define cmp (lambda (a0 a1)
	      (cond
	       ((and (< (cadr a0) (car a1))
		     (< (cadr a0) (cadr a1)))
		#t)
	       (else #f))))
(define top (lambda (a0 a1)
	      (cons (min (car a0) (car a1)) (cons (max (cadr a0) (cadr a1)) '()))))


(define discover(lambda (lat)
		  ((letrec ((x0 (lambda (lat acc)
				  (cond
				   ((null? lat) acc)
				   ((null? acc) (x0 (cdr lat) (cons (car lat) acc)))
				   (else
				    (x1 (cdr lat) acc (car lat) '())))))
			    (x1 (lambda (lat0 lat1 arg0 acc)
				  (cond
				   ((null? lat1) (x0 lat0 (cons arg0 acc)))
				   ((cmp (car lat1) arg0)
				    (x1 lat0 (cdr lat1) arg0 (cons (car lat1) acc)))
				   (else
				    (x1 lat0 (cdr lat1) (top arg0 (car lat1)) acc))))))
		     x0) lat '())))
		
				  

					     
				    
				    
		      
