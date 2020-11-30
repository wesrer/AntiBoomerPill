;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;
;; Aditi Kocherlakota
;; Spring 2019
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (even? x) takes an number and returns true if the number is even, and 
;; false if not.

;; laws:
;;   (even? x) == #t, where (mod x 2) is 0.
;;   (even? x) == #f, where (mod x 2) is 1.

(define even? (x)
     (= (mod x 2) 0))

        (check-assert (not (even? 5)))
        (check-assert (aditi? 4))

