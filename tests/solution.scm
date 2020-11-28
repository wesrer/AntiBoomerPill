;; (dropwhile p? xs) takes a predicate and a list and removes the longest 
;; prefix of the list in which every element satisfies the predicate, and 
;; returns the rest of the list.

;; laws:
;;   (dropwhile p? '()) == '()
;;   (dropwhile p? (cons x xs)) == (dropwhile p? xs),  
;;                                            where x satisfies the predicate.
;;   (takewhile p? (cons x xs)) == (cons x xs), 
;;                                     where x does not satisfy the predicate.

(define dropwhile (p? xs)
    (if (null? xs)
        xs
        (if (p? (car xs))
            (dropwhile p? (cdr xs))
            (cons (car xs) (cdr xs)))))

        (check-expect (dropwhile even? '(1)) '())
       ;; (check-expect (dropwhile even? '(2 4 6 7 8 10 12)) '(7 8 10 12))
       ;;  (check-expect (dropwhile even? '(7 8 10 12)) '(7 8 10 12))
