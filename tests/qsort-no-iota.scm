(define even? (n) (if (= (mod n 2) 0 ) true false))

(define o (f g) (lambda (x) (f (g x))))
(val temp_fun o)
(print ((o not even?) 5))
