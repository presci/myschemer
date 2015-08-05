(define insertsort*
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (insert* (insertsort* (cdr lat)) (car lat))))))



(define insert*
  (lambda (lat x)
    (cond
     ((null? lat) (list x))
     ((<= x (car lat)) (cons x lat ))
     (else
      (cons (car lat) (insert* (cdr lat) x))))))
