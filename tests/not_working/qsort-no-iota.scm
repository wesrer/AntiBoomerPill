(define o (f g) (lambda (x) (f (g x))))

(define qsort (xs)
  (if (null? xs)
      '()
      (let* ([pivot  (car xs)]
             [rest   (cdr xs)]
             [right? (lambda (n) (> n pivot))]
             [left?  (o not right?)])
        (append (qsort (filter left? rest))
                (cons pivot (qsort (filter right? rest)))))))


(check-expect 
  (qsort '(2 1 3)) '(1 2 3))

