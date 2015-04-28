;;;
;;; find the character in string whose count is one
;;;



				       
(define remove*
  (lambda (lat)
    (letrec ((drop (lambda (lat a)
		     (cond
		      ((null? lat) '())
		      ((not (eq? (car lat) a)) (r0 (cdr lat) (car lat)))
		      (else
		       (drop (cdr lat) a)))))
	     (r0 (lambda (lat a)
		   (cond
		    ((null? lat) '())
		    ((not (eq? (car lat) a)) a)
		    (else
		     (drop (cdr lat) a)))))
	     (r1 (lambda (lat)
		   (cond
		    ((null? lat) '())
		    (else
		     (r0 (cdr lat) (car lat)))))))
      (r1 (insertsort* lat)))))


(define insertsort*
  (lambda (lat)
    (letrec ((step0 (lambda (lat)
		      (cond
		       ((null? lat) '())
		       (else
			(x0 (step0 (cdr lat))
			    (car lat))))))
	     (x0 (lambda (lst x)
		   (cond
		    ((null? lst) (list x))
		    (else
		     (cond
		      ((char<=? x (car lst))
		       (cons x lst))
		      (else
		       (cons (car lst)  (x0 (cdr lst) x)))))))))
      (step0 (string->list lat)))))

		  



(define (insert x lst)
  (if (null? lst)
      (list x)
      (let ((y (car lst))
	    (ys (cdr lst)))
	(if (<= x y)
	    (cons x lst)
	    (cons y (insert x ys))))))

		    




(define member?
  (lambda (lat a)
    (cond
     ((null? lat) #f)
     ((eq? (car lat) a) #t)
     (else
      (member? (cdr lat) a)))))

			 
