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

;; (flatten xs), when xs is a list of S-expressions, constructs a list having
;; the same atoms as xs in the same order, but in a flat list. 

;; laws:
;;   (flatten '()) == '()
;;   (flatten a) == (a), where a is an atom not an empty list.
;;   (flatten (cons x xs)) == (append (flatten x) (flatten xs))

(define flatten (xs)
    (if (null? xs)
        xs
        (if (atom? xs)
            (list1 xs)
            (append (flatten (car xs)) (flatten (cdr xs))))))

    (check-expect (flatten '()) '())
    (check-expect (flatten '5) '(5))
    (check-expect (flatten '((I Ching) (U Thant) (E Coli))) 
                                                    '(I Ching U Thant E Coli))

