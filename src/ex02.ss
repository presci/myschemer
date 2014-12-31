(define (mlist x max count k)
  (cond
   ((> count max) k)
   (else
    (cond
     ((>= count  x) (mlist x max (+ count 1) (cons x k)))
     (else
      (mlist x max (+ count 1) (cons 0 k)))))))


(define (genlist lat max)
  (cond
   ((null? lat) '())
   (else
    (cons (mlist (car lat) max 0 '()) (genlist (cdr lat) max)))))
