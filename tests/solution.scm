;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;
;; Aditi Kocherlakota
;; Spring 2019
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;

;;;;  Exercise 2

;; (mirror xs) , when xs is a list of S-expressions, returns a list in which 
;; every list in xs is recursively mirrored, and the resulting lists are in 
;; reverse order.

;; laws: 
;;   (mirror a) == a, where a is an atom.
;;   (mirror (cons x xs)) == (append (mirror xs) (list1 (mirror x)))

(define mirror (xs)
    (if (atom? xs)
        xs
        (append (mirror (cdr xs)) (list1 (mirror (car xs))))))

        (check-expect (mirror '()) '())
        (check-expect (mirror '((a (b 5)) (c d) e)) '(e (d c) ((5 b) a)))
        (check-expect (mirror '(1 2 3 4)) '(4 3 2 1))