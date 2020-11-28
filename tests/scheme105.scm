;; Comp105 scheme assignment
;; You will be implementing these functions.
(check-expect (function? count) #t)
(check-expect (function? countall) #t)
(check-expect (function? mirror) #t)
(check-expect (function? flatten) #t)
(check-expect (function? contig-sublist?) #t)
(check-expect (function? sublist?) #t)
(check-expect (function? takewhile) #t)
(check-expect (function? dropwhile) #t)
(check-expect (function? zip) #t)
;;(check-expect (function? unzip) #t)
(check-expect (function? arg-max) #t)
(check-expect (function? merge) #t)
(check-expect (function? permutation?) #t)
(check-expect (function? split-list) #t)
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
;; Problem 2b
;;
;; (countall x xs) takes an atom and a list and checks if the
;; atom x is present on the members of the list xs (both top 
;; level and recursively)
(check-expect (countall 'a '(1 2 3)) 0) 
(check-expect (countall 'a '(1 a 3)) 1)
(check-expect (countall 'a '((a 1) 3)) 1)
(check-expect (countall 'a '((a 1) (a 2) 3)) 2)
(check-expect (countall 'a '((a 1) (a a 2) 3)) 3)
(define countall (x xs)
  (if (null? xs) 0
                (if (atom? (car xs)) 
                  (if (equal? x (car xs)) (+ 1 (countall x (cdr xs)))
                                    (countall x (cdr xs)))
                  (+ (count x (car xs)) (countall x (cdr xs))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 2c
;;
;; (mirror xs) takes a list xs and returns a list in which
;; every list in xs is recursively mirrored
(check-expect (mirror '()) '())
(check-expect (mirror '(3 2 1)) '(1 2 3))
(check-expect (mirror '(3 2 (1 4))) '((4 1) 2 3))
(define mirror (xs)
  (if (null? xs) '()
        (if (atom? (car xs)) (append (mirror (cdr xs)) (list1 (car xs)))
                       (append (mirror (cdr xs)) (list1 (reverse (car xs))))
                       ))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 2d
;;
;; <contract>
(check-expect (flatten '()) '())
(check-expect (flatten '((a) ((b)))) '(a b))
;; TODO: check this with a TA
;;(check-expect (flatten '(())) '())
(define flatten (xs)
  (if(null? xs) '()
        (if (atom? (car xs)) (cons (car xs) (flatten (cdr xs)))
                        (append (flatten (car xs)) (flatten (cdr xs))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 2e
;;
;; <contract>
(check-expect (contig-sublist? '(a b c) '(x y a b c m n)) #t)
(check-expect (contig-sublist? '(a b c) '(x f a y b c m n)) #f)
(define find-contig (xs ys flag)
    ;; if we have finished scanning through xs
    ;; return the value of the flag
    (if (null? xs) flag
              ;; else if
              (if (equal? (car xs) (car ys))
                    ;; found a matching element
                    ;; recursively call now with a different
                    ;; flag 
                      (find-contig (cdr xs) (cdr ys) #t)
                      ;; if we had found a match the previous
                      ;; time and this time we didn't
                      ;; then there are no contingent blocks
                      ;; So, return false.
                      (if flag #f
                          ;; else
                          ;; if no matches were found both in 
                          ;; this runthrough and the previous,
                          ;; keep scanning with the same flag
                            (find-contig xs (cdr ys) #f)))))
(define contig-sublist? (xs ys)
(if (null? xs) #t
        ;; if xs is not nil but ys is nil
        (if (null? ys) #f
              ;; if both xs and ys is not nil
              (find-contig xs ys #f))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 2f
;;
;; <contract>
(check-expect (sublist? '(a b c) '(x y a z b c m n)) #t)
(check-expect (sublist? '(a b c) '(x f a y b m n)) #f)
;; helper function
(define find-subset (xs ys flag)
    ;; if we have finished scanning through xs
    ;; return the value of the flag
    (if (null? xs) flag
              (if (null? ys) #f 
              ;; else if
              (if (equal? (car xs) (car ys))
                    ;; found a matching element
                    ;; recursively call now with a different
                    ;; flag 
                      (find-subset (cdr xs) (cdr ys) #t)
                          ;; else
                          ;; if no matches were found both in 
                          ;; this runthrough and the previous,
                          ;; keep scanning with the same flag
                            (find-subset xs (cdr ys) flag)))))
(define sublist? (xs ys)
   (find-subset xs ys #f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 10
;;
;; (takewhile p? xs) <does what exactly?> [this is the contract]
(check-expect (takewhile even? '(2 4 6 7 8 10 12)) '(2 4 6)) 
(define even? (x) (= (mod x 2) 0))
(define takewhile (p? xs)
  (if (null? xs)
          xs
          (if (p? (car xs)) (cons (car xs) (takewhile p? (cdr xs)))
                            '())))
;; (dropwhile p? xs) <does what exactly?> [this is the contract]
(check-expect (dropwhile even? '(2 4 6 7 8 10 12)) '(7 8 10 12))
(define dropwhile (p? xs)
  (if (null? xs) xs
            (if (p? (car xs)) (dropwhile p? (cdr xs))
                            xs)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem 30
;;
;;
;; a) find the square root of the sum of two squares
;;
;; b) It runs through the expressions e1 and e2 and 
;;    assigns the resultant values v1 and v2 which are
;;    the functional implementations of the predefined 
;;    functions '*' and '+', and then assigns them to 
;;    the local variables '+' and '*'. When it finally
;;    evaluates e, which is the given expression, it uses
;;    the local values of + and * instead of the predefined 
;;    ones.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem B (zip)
;;
;; <contract>
(check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))
(define zip (xs ys)
    (if (null? xs) '()
          (cons (list2 (car xs) (car ys)) (zip (cdr xs) (cdr ys))))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem B (unzip)
;;
;; <contract>
;;(define unzip-lists (xs lis1))
;;(define unzip (xs))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem C
;;
;; <contract>
(define arg-max (f as)
   (if (null? (cdr as)) as
                  (if (<= (f (car as)) (f (cadr as)))
                      (arg-max (cdr as))
                      (arg-max (cons (car as) (cddr as))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem D
;;
;; <contract>
(check-expect (merge '(4 7 9) '(5 6 8)) '(4 5 6 7 8 9))
(check-expect (merge '(4 5) '()) '(4 5))
(define merge (xs ys)
  (if (null? xs) 
          (cons xs ys)
          ;; else
          (if (null? ys) xs
                  (if (<= (car xs) (car ys)) 
                          ;; the lowest element is already in
                          ;; the right spot
                          (cons (car xs) (merge (cdr xs) ys))
                          ;; rearranging the lowest element
                          (merge (cons (car ys) xs) (cdr ys)) 
                          ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem G
;;
;; permutation? takes in two lists xs and ys and checks whether
;; they are permutations of each other, i.e. whether they both
;; have the exact same elements the exact same number of times
(check-expect (permutation? '(a b c) '(c a b)) #t)
(check-expect (permutation? '(a b c d) '(c a b)) #f)
(check-expect (permutation? '(a b a) '(b a b)) #f)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem H
;;
;; <contract>
(check-expect (split-list '(a b c d)) '((c a) (d b)))
(check-expect (split-list '(a b c d e)) '((d b) (e c a)))
(define list-splitter (xs lis1 lis2)
  (if (null? xs)
        (cons lis2 (cons lis1 '()))
        (list-splitter (cdr xs) (cons (car xs) lis2) lis1)))
(define split-list (xs)
  (list-splitter xs '() '()))