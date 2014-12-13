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







     
