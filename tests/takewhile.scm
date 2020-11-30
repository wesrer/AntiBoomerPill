;; (takewhile p? xs) takes a predicate and a list and returns the 
;; longest prefix of the list in which every element satisfies the predicate. 

;; laws:
;;   (takewhile p? '()) == '()
;;   (takewhile p? (cons x xs)) == (cons x (takewhile p? xs)),
;;                                     where x satisfies the predicate.
;;   (takewhile p? (cons x xs)) == '(), where x does not satisfy the predicate.


(define takewhile (p? xs)
    (if (null? xs)
        xs
        (if (p? (car xs))
            (cons (car xs) (takewhile p? (cdr xs)))
             '())))

;;      (check-expect (takewhile even? '(2 4 6 7 8)) '(2 4 6))
;;        (check-expect (takewhile even? '(3 6 9 8)) '())
        (check-expect (takewhile even? '()) '())
        (check-expect (takewhile even? '(1)) '())