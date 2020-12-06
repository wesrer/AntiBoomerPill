(define test (x) (lambda (y) (lambda (z) (+ x (+ z y)))))

(print (((test 3) 4) 5) )
