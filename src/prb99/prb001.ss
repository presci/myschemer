(define insertsort*
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (insert*  (insertsort* (cdr lat)) (car lat))))))

(define insert*
  (lambda (lat x)
    (cond
     ((null? lat) (list x))
     ((<= x ( car lat)) (cons x lat))
     (else
      (cons (car lat) (insert* (cdr lat) x))))))

     


;;
(define mul-all
  (lambda (lat)    (map (lambda (x) (* x 2)) lat)    ))


;; p18
(define slice
  (lambda (lat a c)
    ((letrec ((x0 (lambda (lat0 e acc)
		    (cond
		     ((null? lat0) acc)
		     ((> e c) acc)
		     ((>= e a) ( x0 (cdr lat0) (+ e 1) (cons (car lat0) acc)))
		     (else
		      (x0 (cdr lat0) (+ e 1) acc))))))
       x0) lat 1 '())))





;;p16
(define drop*
  (lambda (lat c)
    ((letrec ((x0 (lambda (lat0 c0 acc)
		    (cond
		     ((null? lat0) acc)
		     ((eq? c c0) (x0 (cdr lat0) 1 acc))
		     (else
		      (x0 (cdr lat0) (+ c0 1) (cons (car lat0) acc))))))) x0)
     lat 1 '())))




;; p10 run-length encoding of a list
(define encode*
  (lambda (lat)
    ((letrec ((x0 (lambda (lat0 x c acc)
		    (cond
		     ((null? lat0) (cons (list x c)  acc))
		     ((eq? (car lat0) x) (x0 (cdr lat0) x (+ 1 c) acc))
		     (else
		      (x0 (cdr lat0) (car lat0) 1 (cons (list x c) acc)))))))
       x0) (cdr lat) (car lat) 1 '())))
		      





(define last*
  (lambda (lat) ((letrec ((x0  (lambda (lat0)
		 (cond  ((null? (cdr lat0)) (car lat0))
		  (else   (x0 (cdr lat0)))))))
       x0) lat)))

(define div0*
  (lambda (a b)
    ((letrec ((x0   (lambda (x c)
		 (cond ((< x b) c)
		  (else   (x0 (- x b) (+ c 1)))))))  x0) a 0)))
(define div*
  (lambda (a b)
    ((letrec ((x0   (lambda (x c) (cond  ((or ( < x b) (= x 0)) c)  (else  (x0 (- x b) (+ c 1)))))))   x0) a 0)))
