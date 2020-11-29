
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 2a
;;
;; (count x xs) takes an atom and a list and checks if the
;; atom x is present on the members of the list xs (only
;; top level)
(check-expect (count '5 '(1 2)) 0) 
(check-expect (count '1 '(1 a)) 1)
(check-expect (count '1 '(1 (a 1))) 1)  
(define count (x xs)
  (if (null? xs) 0
                (if (equal? x (car xs)) (+ 1 (count x (cdr xs)))
                                    (count x (cdr xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem G
;;
;; permutation? takes in two lists xs and ys and checks whether
;; they are permutations of each other, i.e. whether they both
;; have the exact same elements the exact same number of times
(check-expect (permutation? '(a b c) '(c a b)) #t)
(check-expect (permutation? '(a b c d) '(c a b)) #f)

;; Helper function is necessary to skip the length check at 
;; every recursive step
(define is-permutation? (xs ys)
    (if (null? xs) #t
            (if (equal? (count (car xs) xs) (count (car xs) ys))
                  (is-permutation? (cdr xs) ys)
                  #f)))
(define permutation? (xs ys)
  (if (equal? (length xs) (length ys))
          (is-permutation? xs ys)
          ;; lengths of xs and ys are unequal
          #f))
