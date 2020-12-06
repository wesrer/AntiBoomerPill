(define x (m) (lambda (y) m))
(check-expect ((x 7) 0) 7)
