
;;; P03 find the kth element in list
(define kth
  (lambda (lat a)
    ((letrec ((r0
	      (lambda (lat g)
		(cond
		 ((null? lat) #f)
		 ((= g a) (car lat))
		 (else
		  (r0 (cdr lat) (+ g 1)))))))
       r0) lat 0)))


;;; P04 find the number of element in list
(define lth
  (lambda (lat)
    ((letrec ((r0
	       (lambda (lat g)
		 (cond
		  ((null? lat) g)
		  (else
		   (r0 (cdr lat) (+ g 1)))))))
       r0) lat 0)))
;;; Reverse a list
(define rev0
  (lambda (lat)
    (letrec ((r0
	      (lambda (lat0 lat1)
		(cond
		 ((null? lat0) lat1)
		 (else
		  (r0 (cdr lat0) (cons (car lat0) lat1)))))))
      (r0 lat '())
      )))


;;; is the list palindrome      
(define eq0?
  (lambda (lat)
    ((letrec ((r0 (lambda (lat0 lat1)
		    (cond
		     ((null? lat0) #t)
		     ((not (= (car lat0) (car lat1))) #f)
		     (else
		      (r0 (cdr lat0) (cdr lat1)))))))
       r0) lat (rev0 lat))))

;;; Find unique in 2 sets
(define minus0
  (lambda (lat0 lat1)
    ((letrec ((r0
	       (lambda (lat0 lat1 acc)
		 (cond
		  ((null? lat0) (a0 lat1 acc))
		  ((null? lat1) (a0 lat0 acc))
		  ((> (car lat0) (car lat1)) (r0 lat0 (cdr lat1) (cons (car lat1) acc)))
		  ((< (car lat0) (car lat1)) (r0 (cdr lat0) lat1 (cons (car lat0) acc)))
		  (else
		   (r0 (cdr lat0) (cdr lat1) acc)))))
	      (a0 (lambda (lat acc)
		    (cond
		     ((null? lat) acc)
		     (else
		      (a0 (cdr lat) (cons (car lat) acc))))))
	      )
       r0) lat0 lat1 '())))
	       
		  
