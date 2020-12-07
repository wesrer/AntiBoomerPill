;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 1

;; (is-prefix-of? (xs ys)) Returns whether xs is a prefix of list ys

;; (is-prefix-of? '() ys)                  = #t
;; (is-prefix-of? (cons x xs) '())         = #f
;; (is-prefix-of? (cons x xs) (cons y ys)) = (&& (= x y) (is-prefix-of? xs ys))

(check-assert (null? '()))
(check-assert (not (null? '(1 2))))
;; (check-assert (pair? '(1 2 3)))
(check-assert (pair?  (cons 1 2)))
(define is-prefix-of? (xs ys) (|| (null? xs)
                                  (&& (&& (pair? ys) (= (car xs) (car ys)))
                                      (is-prefix-of? (cdr xs) (cdr ys)))))

        (check-assert (is-prefix-of? '(1) '(1 2 3 4)))
        (check-assert (is-prefix-of? '(1) '(1 3)))
        (check-assert (is-prefix-of? '() '(1 2 3 4)))
        (check-assert (is-prefix-of? '() '()))
        (check-assert (not (is-prefix-of? '(2) '(1 2 3 4))))



;; (contig-sublist? xs ys) Returns whether xs is a contiguous sublist of ys

;; laws:
;;   (contig-sublist? (cons x xs)   '()) == #f
;;   (contig-sublist?  xs   (cons y ys)) ==  (|| (is-prefix-of? xs (cons y ys))
;;                                               (is-prefix-of? xs         ys))
;; 
(define contig-sublist? (xs ys) (|| (is-prefix-of? xs ys)
                                    (&& (not (null? ys))
                                        (is-prefix-of? xs (cdr ys)))))

        (check-assert (contig-sublist? '() '()))
        (check-assert (contig-sublist? '() '(1)))
        (check-assert (contig-sublist? '() '(1 2)))
        (check-assert (contig-sublist? '(1) '(1)))
        (check-assert (contig-sublist? '(1) '(2 1)))
        (check-assert (contig-sublist? '(1) '(2 1 3)))
        (check-assert (contig-sublist? '(a y b) '(x a y b z c)))
        (check-assert (contig-sublist? '(x) '(x a y b z c)))
        (check-assert (not (contig-sublist? '(1) '())))
        (check-assert (not (contig-sublist? '(a b c) '(x a y b z c))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 8


;; (mirror xs) Returns a list in which every list in xs is recursively mirrored
;; and the resulting lists are in reverse order.
;;
;; laws:
;;    (mirror (cons x xs)) == ()
;;    (mirror         xs)) == xs when (null? xs) or (atom? xs)
;; (define mirror (xs) 
;;         (if (|| (null? xs) (atom? xs))
;;                 xs
;;                 (append (mirror (cdr xs)) (list1 (mirror (car xs))))))

;;         (check-expect (mirror '()) '())
;;         (check-expect (mirror '(1 2 3 4 5)) '(5 4 3 2 1))
;;         (check-expect (mirror '((a (b 5)) (c d) e)) '(e (d c) ((5 b) a)))


;; ;; (flatten xs) Function flatten consumes a list of S-expressions and erases
;; ;; internal parentheses. That is. when xs is a list of S-expressions, 
;; ;; 
;; ;; laws flatten:
;; ;;   (flatten '())         == '()
;; ;;   (flatten x)           == (list1 x) where x is an atom
;; ;;   (flatten (cons x xs)) == (append (flatten x) (flatten xs))
;; ;;

;; (define flatten (xs) 
;;         (if (null? xs) 
;;                 '() 
;;                 (if (atom? xs) 
;;                         (list1 xs) 
;;                         (append (flatten (car xs)) (flatten (cdr xs))))))



;;         (check-expect (flatten '(1 2 3 4 5)) '(1 2 3 4 5))
;;         (check-expect (flatten '((a (b 5)) (c d) e)) '(a b 5 c d e))
;;         (check-expect (flatten '(((((a)))))) '(a))
;;         (check-expect (flatten '((()))) '())
;;         (check-expect (flatten '((((((((()))))))))) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 31


;; (takewhile p? xs) Function takewhile takes a predicate and a list and
;; and returns the longest prefix of the list in which every element 
;; satisfies the predicate.

;; laws:
;;   (takewhile p? '())          == '().
;;   (takewhile p?  (cons x xs)) == '()                       when (not (p? x))
;;   (takewhile p?  (cons x xs)) == (cons x (takewhile p? xs)) when p? x
;;
(define takewhile (pred? xs)
        (if (|| (null? xs) (not (pred? (car xs))))
                '()
                (cons (car xs) (takewhile pred? (cdr xs)))))
        


        ;; (check-expect (takewhile null? '()) '())
        ;; (check-expect (takewhile pair? '()) '())
        
        ;; (check-expect (takewhile pair? '(() a)) '())

        ;; (check-expect (takewhile null? '(() () () () a () ())) '(() () () () ))

;; (dropwhile p? xs) Function dropwhile removes the longest prefix and returns 
;; whatever is left over.

;;   (dropwhile p? '())          == '().
;;   (dropwhile p?  (cons x xs)) == (cons x xs)       when (not (p? x))
;;   (dropwhile p?  (cons x xs)) == (dropwhile p? xs) when p? x
;;

;; (define dropwhile (pred? xs)
;;         (if (|| (null? xs) (not (pred? (car xs))))
;;                 xs ; xs = '() if null? xs
;;                 (dropwhile pred? (cdr xs))))

;;         (define even? (x) (= (mod x 2) 0))
;;         (check-expect (dropwhile even? '(0 2 4 6 7 8 10 12)) '(7 8 10 12))
;;         (check-expect (dropwhile even? '()) '())
;;         (check-expect (dropwhile even? '(1 2 4 6 7 8 10 12)) 
;;                      '(1 2 4 6 7 8 10 12))
        



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise B


;; (take n xs) expects a natural number and a list of values. It returns the
;;             longest prefix of xs that contains at most n elements.
;;
;; The laws for take are nearly identical to the laws for takewhile
;;
;; laws:
;;   (take n '())                == '()
;;   (take 0  (cons x xs))       == '()
;;   (take (+ n 1)  (cons x xs)) == (cons x (take n xs)) 
(define take (n xs)
        (if (|| (null? xs) (= n 0))
                '()
                (cons (car xs) (take (- n 1) (cdr xs)))))

        
        (check-expect (take 1000 '()) '())
        (check-expect (take 0 '(1 2 3 4 5 6 8 9))  '())
        (check-expect (take 4 '(1 2 3 4)) '(1 2 3 4) )
        (check-expect (take 2 '(1 2 3 4)) '(1 2) )
        (check-expect (take 5 '(1 2 3 4)) '(1 2 3 4) ) 


;; (drop n xs) Drop expects a natural number and a list of values. Roughly, 
;;             it removes n elements from the front of the list. 

;; laws:
;;   (drop n '())                == '().
;;   (drop 0  xs)                == xs
;;   (drop (+ n 1)  (cons x xs)) == (drop n xs) 
(define drop (n xs)
        (if (|| (null? xs) (= n 0))
                xs ; xs = '() if null? xs
                (drop (- n 1) (cdr xs))))

        
        (check-expect (drop 1000 '()) '())
        (check-expect (drop 0 '(1 2 3 4 5 6 8 9))  '(1 2 3 4 5 6 8 9))
        (check-expect (drop 4 '(1 2 3 4)) '())
        (check-expect (drop 2 '(1 2 3 4)) '(3 4) )
        (check-expect (drop 5 '(1 2 3 4)) '() ) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise C


;; (zip xs ys) Function zip converts a pair of lists to a list of pairs by 
;; associating corresponding values in the two lists. (If zip is given lists 
;; of unequal length, its behavior is not specified.) 

;; laws:
;;   (zip '() '())                 = '()
;;   (zip (cons y ys) (cons x xs)) = (cons (list2 x y) (zip ys xs))
;;

;; (define zip (xs ys)
;;         (if (null? xs) ; ys must be null? by the function contract
;;                 '()
;;                 (cons (list2 (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

        
;;         (check-expect (zip '() '()) '() )
;;         (check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)) )
;;         (check-expect (zip '(11 11 15) '(Guyer Sheldon Korman)) 
;;                                        '((11 Guyer) (11 Sheldon) (15 Korman)))


;; ;; (unzip ps) Function unzip converts a list of pairs to a pair of lists. In 
;; ;; both functions, a “pair” is represented by a list of length two, e.g., a 
;; ;; list formed using predefined function list2.
;; ;;
;; ;; laws unzip
;; ;;   (unzip '()) = '(() ())
;; ;;   (unzip  ps) = (let ([x (car ps)] [as (unzip (cdr ps))]) 
;; ;;                      (list2  (cons (car  x) (car  as))
;; ;;                              (cons (cadr x) (cadr as))))))

;; (define unzip (ps) 
;;         (if (null? ps) 
;;                 '(() ())
;;                  (let ([x (car ps)] [as (unzip (cdr ps))]) 
;;                         (list2  (cons (car  x) (car  as))
;;                                 (cons (cadr x) (cadr as))))))

        
;;         (check-expect (unzip '((I Magnin) (U Thant) (E Coli))) 
;;                       '((I U E) (Magnin Thant Coli)))
;;         (check-expect (unzip '()) '(() ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise D


;; (arg-max f xs) Function arg-max expects two arguments: a function f that 
;; maps a value in set A to a number, and a nonempty list as of values in set 
;; A. It returns an element a in as for which (f a) is as large as possible.)

;; laws unnamed helper lambda (l as a place(holder):
;;  (l x (old-f-x old-x)) =  (list2 (f x) x) when (> (f x) old-f-x)
;;  (l x (old-f-x old-x)) = '(old-f-x old-x) otherwise
;;
;;
;; laws arg-max:
;;   (arg-max f (cons x xs)) == (cadr (foldl l (list2 (f x) x) xs))
;;   
;;

;; (define arg-max (f xs)
;;         (cadr
;;                 (foldl
;;                         (lambda (x a)
;;                                 (if (> (f x) (car a))
;;                                         (list2 (f x) x)
;;                                         a))
;;                         (list2 (f (car xs)) (car xs))
;;                         (cdr xs))))

        
;;         (define pow2  (x) (* x x))
;;         (define pow3  (x) (* (pow2 x) x))
;;         (define pow4  (x) (* (pow2 x) (pow2 x)))
;;         (check-expect (arg-max pow2 '(1 2 3 4 5 6 7 8 9 -10)) -10)
;;         (check-expect (arg-max pow3 '(1 2 3 4 5 6 7 8 9 -10))   9)
;;         (check-expect (arg-max pow4 '(1 2 3 4 5 6 7 8 9 -10)) -10) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise E


;; (record point [x y])

;; (rightmost-point ps) Define a function rightmost-point that takes a nonempty
;; list of point records and returns the one with the largest x coordinate. 
;; Break ties arbitrarily.

;; laws:
;;   (rightmost-point ps) == (arg-max point-x ps)
;;
;; (define rightmost-point (ps) (arg-max point-x ps))

;;         (check-expect (rightmost-point 
;;                                 (list3 
;;                                         (make-point  1 2)
;;                                         (make-point  3 5)
;;                                         (make-point -5 2))) 
;;                         (make-point  3 5))
;;         (check-expect (rightmost-point 
;;                                 (list1 
;;                                         (make-point -1000 2))) 
;;                         (make-point -1000 2))
        
