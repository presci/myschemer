(define compare (lambda (x0 x1)
  (let ((e0 x0) (e1 x1))
    (cond
     ((< e0 e1) e0)
     (else e1)))))
