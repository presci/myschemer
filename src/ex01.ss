

(define even?
  (lambda (a)
    (cond ((= (modulo a 2) 0) #t)
	  (else
	   #f))))

(define (myfilter func lat)
  (cond
   ((null? lat ) '())
   ((even? (car lat))
	   (cons (car lat) (myfilter func (cdr lat))))
    (else
     (myfilter func (cdr lat)))))

(define 2*
  (lambda (a)
    (* 2 a)))

(define (mymap func lat)
  (cond
   ((null? lat) '())
   (else
    (cons (func (car lat)) (mymap func (cdr lat))))))



(define (flatten lat)
  (cond
   ((null? lat) '())
   ((pair? (car lat))
    (cons (flatten (car lat)) (flatten (cdr lat))))
   (else
    (cons (car lat) (flatten (cdr lat))))))


(define (getfirst lat)
  (cond
   ((null? lat) '())
   ((pair? (car lat)) (getfirst (car lat)))
   (else
    (cons (car lat) '()))))



(define (atom? x)
  (and (not (null? x))
              (not (pair? x))))


;; hello world
(define (hello)
  (display "hello world")
  (newline)
  )

;; remove the first occurrence

(define (rember x lat)
  (cond ((null? lat) (quote()))
	(else (cond
	       ((eq? (car lat) x) (cdr lat))
	       (else (cons (car lat)
			   (rember x
				   (cdr lat))))))))

(define (insert-r new old lat)
  (cond
   ((null? lat) '())
   ((eq? old (car lat))
    (cons old (cons new (cdr lat))))
   (else
    (cons (car lat) (insert-r  new old (cdr lat))))))
		
(define (insert-l new old lat)
  (cond
   ((null? lat) '())
   ((eq? old (car lat))
    (cons new (cons old (cdr lat))))
   (else
    (cons (car lat) (insert-l new old (cdr lat))))))


(define (first lat)
  (cond
   ((null? lat) '())
   (else
    (cons (car (car lat)) (first (cdr lat))))))


;; the subst2 function substitutes the first occurrence
;; of the element o1 or o2 with new in list lat

(define (subst new o1 o2 lat)
  (cond
   ((null? lat) '())
   ((or (eq? o1 (car lat))
	(eq? o2 (car lat)))
       (cons new (cdr lat)))
   (else
    (cons (car lat) (subst new o1 o2 (cdr lat))))))


;; division
(define (div x y z)
  (cond
   ((>= (- x y) 0) (div (- x y) y (+ 1 z)))
   (else z)))


(define (add1 n)
  (+ n 1))



(define (occur x lat)
  (cond
   ((null? lat) 0)
   ((eq? (car lat) x) 
    (add1 (occur x (cdr lat))))
   (else
    (occur x (cdr lat)))))


;(define length
;  (lambda (L)
;    (define length1
;      (lambda (L n)
;	(if (null? L)
;	    n
;	    (length1 (tail L) (+ n 1)))))
;    (length1 L 0)))

    

(define (length lat)
  (define (length1 n lat)
    (cond
     ((null? lat) n)
     (else
      (length1 (+ n 1) (cdr lat)))))
  (length1 0 lat))

(define (sum lat)
  (define (sum1 n lat)
    (cond
     ((null? lat) n)
     (else (sum1 (+ n (car lat)) (cdr lat)))))
  (cond
   ((null? lat) 0)
   (else
    (sum1 0 lat))))

(define (test a)
  (cond
   ((= a 10)
    (display "hello world")
    (display "hello world")
    )
   (else
    (display "newline")
    (display "newline"))))





     
