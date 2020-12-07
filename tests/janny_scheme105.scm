;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 2

;; (reverse_list xs ys) This function reverses all contents of xs 
;;                      and adds it to the front of ys.

;; laws:
;;    (reverse_list '() ys) == ys
;;    (reverse_list (cons a as) ys) == (reverse_list as (cons a ys)),
;;                                      if a is an atom
;;    (reverse_list (cons a as) ys) == (reverse_list as 
;;                                     (cons (reverse_list a '()) ys)),
;;                                     if a is a list

(define reverse_list (xs ys)
    (if (null? xs)
        ys
        (if (atom? (car xs))
            (reverse_list (cdr xs) (cons (car xs) ys))
            (reverse_list (cdr xs) (cons (reverse_list (car xs) '()) ys)))))

        (check-expect (reverse_list '(1 2 3) '()) '(3 2 1))
        (check-expect (reverse_list '((2 4) b a) '(5 6 7)) '(a b (4 2) 5 6 7))
        (check-expect (reverse_list '() '(m u g)) '(m u g))

;; (mirror xs) This function takes in a list of S-expressions and reverses
;;             all the contents of the list. If input contains lists, then
;;             the lists are also reversed.

;; laws:
;;   (mirror '()) == '()
;;   (mirror (cons x xs)) == (reverse_list xs '())

(define mirror (xs)
    (if (null? xs)
        '()
        (reverse_list xs '())))

        (check-expect (mirror '(1 2 3 4)) '(4 3 2 1))
        (check-expect (mirror '()) '())
        (check-expect (mirror '((a (b 5)) (c d) e)) '(e (d c) ((5 b) a)))


;; (flatten xs) Given a list of S-expressions, this function will return a
;;              list with all internal parentheses removed.

;; laws:
;;   (flatten '()) == '()
;;   (flatten (cons x xs)) == (append (list1 x) (flatten xs)), if x
;;                            is an atom
;;   (flatten (cons x xs)) == (append (flatten x) (flatten xs)), if x
;;                            is a list

(define flatten (xs)
    (if (null? xs)
        '()
        (if (atom? (car xs))
                (append (list1 (car xs)) (flatten (cdr xs)))
                (append (flatten (car xs)) (flatten (cdr xs))))))

        (check-expect (flatten '()) '())
        (check-expect (flatten '(a (b 5))) '(a b 5))
        (check-expect (flatten '((a (b 5)) (c d) e)) '(a b 5 c d e))

;; (contig-continue? xs ys) This function returns true if list xs is a 
;;                          contiguous subsequence of list ys and the first 
;;                          atom in xs matches with the first atom in ys. 
;;                          Returns false otherwise. This function is called
;;                          under the premise that the first atom in xs is 
;;                          found in ys and compares subsequent atoms in xs
;;                          and ys.

;; laws:
;;   (contig-continue? '() ys) == #t
;;   (contig-continue? (cons x xs) '()) == #f
;;   (contig-continue? (cons x xs) (cons y ys)) == (contig-continue? xs ys),
;;                                                  where (= y x)
;;   (contig-continue? (cons x xs) (cons y ys)) == #f, where (not (= y x))
(define contig-continue? (xs ys)
    (if (null? xs)
        #t
        (if (null? ys)
            #f
            (if (= (car xs) (car ys))
                (contig-continue? (cdr xs) (cdr ys))
                #f))))

        (check-assert (contig-continue? '() '(a x b)))
        (check-assert (not (contig-continue? '(x) '())))
        (check-assert (contig-continue? '(a x) '(a x b)))
        (check-assert (not (contig-continue? '(x b) '(a x b))))

;; (contig-sublist? xs ys) This function returns true if the list of atoms xs
;;                         is a contiguous subsequence of ys. False, if
;;                         otherwise.

;; laws:
;;   (contig-sublist? '() ys) == #t
;;   (contig-sublist? (cons x xs) '()) == #f
;;   (contig-sublist? (cons x xs) (cons y ys)) == (contig-continue? xs ys),
;;                                                 where (= x y)
;;   (contig-sublist? (cons x xs) (cons y ys)) == (contig-sublist? (cons x xs)
;;                                                 ys), where (not (= x y))

(define contig-sublist? (xs ys)
    (if (null? xs)
        #t
        (if (null? ys)
            #f
            (if (= (car xs) (car ys))
                (contig-continue? (cdr xs) (cdr ys))
                (contig-sublist? xs (cdr ys))))))

(define contig-sublist-updated? (xs ys)
    (if (null? xs)
        #t
        (if (null? ys)
            #f
            (if (and (= (car xs) (car ys)) (contig-continue? (cdr xs) (cdr ys)))
                #t
                (contig-sublist-updated? xs (cdr ys))))))

        (check-assert (not (contig-sublist? '(a b c) '(x a y b z c))))
        (check-assert (contig-sublist? '(a y b) '(x a y b z c)))
        (check-assert (not (contig-sublist? '(x) '())))
        (check-assert (contig-sublist? '() '(x m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 10


;; (takewhile p? xs) Finds the longest prefix of the list that satisfies
;;                   the condition p.

;; laws:
;;   (takewhile p? '()) == '()
;;   (takewhile p? (cons x xs)) == (cons x (takewhile p? xs)), 
;;                                 where (p? x) == #t
;;   (takewhile p? (cons x xs)) == '(), where (p? x) == #f

(define takewhile (p? xs)
    (if (null? xs)
        '()
        (if (p? (car xs))
            (cons (car xs) (takewhile p? (cdr xs)))
            '())))

;; (dropwhile p? xs) This function removes the longest prefix and returns
;;                   whatever is left over.

;; laws:
;;   (dropwhile p? '()) == '()
;;   (dropwhile p? (cons x xs)) == (cons x xs), where (p? x) == #f
;;   (dropwhile p? (cons x xs)) == (dropwhile p? xs), where (p? x) == #t

(define dropwhile (p? xs)
    (if (null? xs)
        '()
        (if (not (p? (car xs)))
            xs
            (dropwhile p? (cdr xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise B


;; (take n xs) Returns a list containing, at most, the first n elements
;;             of xs. n must be a natural number.

;; laws:
;;   (take 0 xs) == '()
;;   (take n '()) == '()
;;   (take n (cons x xs)) == (cons x (take (- n 1) xs)

(define take (n xs)
    (if (|| (= n 0) (null? xs))
        '()
        (cons (car xs) (take (- n 1) (cdr xs)))))

        (check-expect (take 1 '(5 5 3 2 4 11)) '(5))
        (check-expect (take 7 '(5 5 3 2 4 11)) '(5 5 3 2 4 11))
        (check-expect (take 0 '(5 5 3 2 4 11)) '())
        (check-expect (take 5 '()) '())

; ;; (drop n xs) Given that n is a natural number, this function removes
; ;;             the first n elements from the front of the list xs and 
; ;;             returns the rest of the list.

; ;; laws:
; ;;   (drop 0 xs) == xs
; ;;   (drop n '()) == '()
; ;;   (drop n (cons x xs)) == (drop (- n 1) xs)

(define drop (n xs)
    (if (|| (= n 0) (null? xs))
        xs
        (drop (- n 1) (cdr xs))))

        (check-expect (drop 3 '(3 6 7 2)) '(2))
        (check-expect (drop 6 '(1 2 3 4 5)) '())
        (check-expect (drop 0 '(2 6 3 1)) '(2 6 3 1))
        (check-expect (drop 5 '()) '())

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;
; ;;;;  EVERYTHING BEYOND THIS IS COMMENTED OUT DUE TO ERRORS FROM SVM.


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;
; ;;;;  Exercise C


; ;; (zip xs ys) Given that xs and ys are of the same length, this function 
; ;;             converts a pair of lists to a list of pairs, associating
; ;;             corresponding values in xs and ys.

; ;; laws:
; ;;   (zip '() '()) == '()
; ;;   (zip (cons x xs) (cons y ys)) == (cons (list2 x y) (zip xs ys))

(define zip (xs ys)
    (if (&& (null? xs) (null? ys))
        '()
        (cons (list2 (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

        (check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))
        (check-expect (zip '() '()) '())

;; (create-key-list ps) Given an association list ps, this function
;;                      constructs a list containing its keys.

;; laws:
;;   (create-key-list '()) == '()
;;   (create-key-list (cons p ps)) == (cons (alist-first-key (cons p ps))
;;                                       (create-key-list ps))

(define create-key-list (ps)
    (if (null? ps)
        '()
        (cons (alist-first-key ps) (create-key-list (cdr ps)))))

        (check-expect (create-key-list '((11 Guyer) (11 Sheldon)
                                         (15 Korman))) 
                                         '(11 11 15))
        (check-expect (create-key-list '()) '())

; ;; (create-attribute-list ps) Given an association list ps, this function
; ;;                            constructs a list containing its attributes.

; ;; laws:
; ;;   (create-key-list '()) == '()
; ;;   (create-key-list (cons p ps)) == (cons (alist-first-attribute (cons p ps))
; ;;                                                        (create-key-list ps))

(define create-attribute-list (ps)
    (if (null? ps)
        '()
        (cons (alist-first-attribute ps) (create-attribute-list (cdr ps)))))

        (check-expect (create-attribute-list '((11 Guyer) (11 Sheldon)
                                               (15 Korman))) 
                                               '(Guyer Sheldon Korman))
        (check-expect (create-attribute-list '()) '())

;; (unzip ps) This function converts a list of pairs to a pair of lists.

;; laws:
;;   (OMITTED)

(define unzip (ps)
    (if (null? ps)
        '()
        (list2 (create-key-list ps) (create-attribute-list ps))))

        (check-expect (unzip '((I Magnin) (U Thant) (E Coli))) 
                      '((I U E) (Magnin Thant Coli)))
        (check-expect (unzip '((11 Guyer) (11 Sheldon) (15 Korman))) 
                      '((11 11 15) (Guyer Sheldon Korman)))
        (check-expect (unzip '()) '())

(define unzip2 (ps)
    (if (null? ps)
        '(() ())
        (list2 (cons (caar ps) (car (unzip2 (cdr ps)))) (append (cdar ps) (cadr (unzip2 (cdr ps)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise LP


;; (singleton? zs) Returns true if the length of the list is 1.
;;                 False otherwise.

;; laws:
;;   (singleton? '()) == #f
;;   (singleton? (cons x xs)) == (null? xs)
(define singleton? (zs)
    (if (null? zs)
        #f
        (null? (cdr zs))))

        (check-assert (not (singleton? '())))
        (check-assert (singleton? '(x)))
        (check-assert (not (singleton? '(a b c d))))



;; (has-n-elements? xs n) Returns true if the length of the list xs is
;;                        exactly equal to n.

;; laws:
;;   (has-n-elements? '() 0) == #t
;;   (has-n-elements? (cons x xs) 0) == #f
;;   (has-n-elements? '() n) == #f
;;   (has-n-elements? (cons x xs) n) == (has-n-elements? xs (- n 1))

(define has-n-elements? (xs n)
    (if (&& (null? xs) (= n 0))
        #t
        (if (|| (null? xs) (= n 0))
            #f
            (has-n-elements? (cdr xs) (- n 1)))))

        (check-assert (has-n-elements? '(1 2 3 4 a b c) 7))
        (check-assert (not (has-n-elements? '(1 2 3 4 a b c) 8)))
        (check-assert (not (has-n-elements? '() 7)))
        (check-assert (not (has-n-elements? '(m n p) 2)))
        (check-assert (has-n-elements? '() 0))

; ;; (nearly-same-lengths? as bs) Returns true if the lengths of the lists differ
; ;;                              by at most 1.

; ;; laws:
; ;;   (nearly-same-lengths? '() '()) == #t
; ;;   (nearly-same-lengths? (cons a as) '()) == (singleton? (cons a as))
; ;;   (nearly-same-lengths? '() (cons b bs)) == (singleton? (cons b bs))
; ;;   (nearly-same-lengths? (cons a as) (cons b bs)) == 
; ;;                                          (nearly-same-lengths? as bs)

(define nearly-same-lengths? (as bs)
    (if (&& (null? as) (null? bs))
        #t
        (if (|| (null? as) (null? bs))
            (|| (singleton? as) (singleton? bs))
            (nearly-same-lengths? (cdr as) (cdr bs)))))

        (check-assert (nearly-same-lengths? '() '()))
        (check-assert (not (nearly-same-lengths? '() '(a b s))))
        (check-assert (nearly-same-lengths? '(a) '()))
        (check-assert (nearly-same-lengths? '(1 2 3) '(4 5 6 7)))
        (check-assert (not (nearly-same-lengths? '(a b o r e) '(p b m))))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;
; ;;;;  Exercise D

; ;; (find-max f xs max) Given a function f, a list xs, and a starting
; ;;                     maximum value max, this function returns the
; ;;                     element that evaluates to the greatest value
; ;;                     when f is applied to it.

; ;; laws:
; ;;   (find-max f '() max) == max
; ;;   (find-max f (cons x xs) max) == (find-max f xs x), if
; ;;                                       (> (f x) (f max))
; ;;   (find-max f (cons x xs) max) == (find-max f xs max), if
; ;;                                       (< (f x) (f max))

(define find-max (f xs max)
    (if (null? xs)
        max
        (if (> (f (car xs)) (f max))
            (find-max f (cdr xs) (car xs))
            (find-max f (cdr xs) max))))


; ;; (arg-max f xs) Given function f and a nonempty list xs, this function will
; ;;                return an element a in which (f a) is the largest.

; ;; laws:
; ;;   (arg-max f (cons x xs)) == (find max (f xs x))

(define arg-max (f xs)
    (find-max f (cdr xs) (car xs)))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;
; ;;;;  Exercise E

; (record point (x y))

;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;
; ;;;;  Exercise F


; ;; (remove-one-copy x xs) Given an S-expression x and a list of S-expressions
; ;;                        xs, this function removes the first instance of x 
; ;;                        from xs. It is an error if x is not found in xs.

; ;; laws:
; ;;   (remove-one-copy x '()) == (error)
; ;;   (remove-one-copy x (cons s sxs)) == xs, if (equals? x s)
; ;;   (remove-one-copy x (cons s sxs)) == (cons s (remove-one-copy x sxs)),
; ;;                                       if (not (equals? x s))

; (define remove-one-copy (x xs)
;     (if (null? xs)
;         (error)
;         (if (equal? (car xs) x)
;             (cdr xs)
;             (cons (car xs) (remove-one-copy x (cdr xs))))))

;         (check-expect (remove-one-copy 'a '(a a b b c c)) '(a b b c c))
;         (check-expect (remove-one-copy '(b c) '((a b) (b c) (c d))) 
;                       '((a b) (c d)))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;
; ;;;;  Exercise G

; ;; (member? x xs) Returns true if element x is found in list xs. False,
; ;;                if otherwise.

; ;; laws:
; ;;   (member? x '()) == #f
; ;;   (member? x (cons y ys)) == #t, if (equal? x y)
; ;;   (member? x (cons y ys)) == (member? x ys), if (not (equal? x y))

(define member? (x xs)
    (if (null? xs)
        #f
        (if (equal? x (car xs))
            #t
            (member? x (cdr xs)))))

        (check-assert (member? 'a '(b a c)))
        (check-assert (not (member? 'z '(a b c))))
        (check-assert (not (member? 'z '())))

; ;; (permutation? xs ys) Returns true if lists xs and ys have the same elements
; ;;                      but not necessarily in the same order. False, if
; ;;                      otherwise.

; ;; laws:
; ;;   (permutation? '() '()) == #t
; ;;   (permutation? (cons x xs) (cons y ys)) == (permutation? 
; ;;                                             (remove-one-copy x xs)
; ;;                                             (remove-one-copy x ys)),
; ;;                                             where (member? x ys)
; ;;   (permutation? (cons x xs) (cons y ys)) == #f, where (not (member? x ys))

; (define permutation? (xs ys)
;     (if (and (null? xs) (null? ys))
;         #t
;         (if (member? (car xs) ys)
;             (permutation? (remove-one-copy (car xs) xs) 
;                           (remove-one-copy (car xs) ys))
;             #f)))

;         (check-assert (permutation? '(a b c) '(c b a)))
;         (check-assert (permutation? '() '()))
;         (check-assert (not (permutation? '(a b b) '(a a b))))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;
; ;;;;  Exercise H

; ;; (even-out xs ys) This function will even out lengths of the two lists, 
; ;;                  xs and ys, and return a list containing the evened out 
; ;;                  lists. xs should contain more elements than ys.

; ;; laws:
; ;;   (even-out (cons x xs) (cons y ys)) == (list2 xs ys), if (nearly-same-
; ;;                                         lengths? (cons x xs) (cons y ys))
; ;;   (even-out (cons x xs) (cons y ys)) == (even-out xs (cons x ys)), if
; ;;                                         (not (nearly-same-lengths?
; ;;                                         (cons x xs) (cons y ys)))

(define even-out (xs ys)
    (if (nearly-same-lengths? xs ys)
        (list2 xs ys)
        (even-out (cdr xs) (cons (car xs) ys))))

        (check-expect (even-out '(a b c) '(1 2 3)) '((a b c) (1 2 3)))
        ; (check-assert (permutation? '(3 5 8 2 5 3 2 8 0 9) 
        ;               (append (car (even-out '(3 5 8 2 5 3 2 8 0 9) '()))
        ;                       (cadr (even-out '(3 5 8 2 5 3 2 8 0 9) '())))))

; ;; (split-list originals) This function takes a list xs and splits it into
; ;;                        two lists of nearly equal length. It will return
; ;;                        a two-element list.

; ;; laws:
; ;;   (split-list '()) == ('() '())
; ;;   (split-list (cons x xs)) == (even-out originals '())

(define split-list (originals)
    (even-out originals '())) 

        (check-expect (split-list '()) '(() ()))
        (check-expect (split-list '(a b)) '((b) (a)))
        (check-expect (split-list '(a b c)) '((b c) (a)))


; ;; These are functions given in the specifications to test the functionality
; ;; of split-list:

(define split-list-returns-two? (xs)
    (let ([result (split-list xs)])
        (has-n-elements? result 2)))

; (define split-list-splits? (xs)
;     (&& (split-list-returns-two? xs)
;         (let ([result (split-list xs)])
;             (permutation? xs (append (car result) (cadr result))))))

; (define split-list-splits-evenly? (xs)
;     (&& (split-list-splits? xs)
;         (let ([result (split-list xs)])
;             (nearly-same-lengths? (car result) (cadr result)))))

        (check-assert (split-list-returns-two? '(a b c d e 1 5 3)))
        ; (check-assert (split-list-splits? '(a b c d e 1 5 3)))
        ; (check-assert (split-list-splits-evenly? '(a b c d e 1 5 3)))


; ;; (merge xs ys) This function will merge two lists of sorted numbers and
; ;;               will return a list of numbers sorted in increasing order.

; ;; laws:
; ;;   (merge '() ys) == ys
; ;;   (merge xs '()) == xs
; ;;   (merge (cons x xs) (cons y ys)) == (cons x (merge xs (cons y ys))),
; ;;                                      if (< x y)
; ;;   (merge (cons x xs) (cons y ys)) == (cons y (merge (cons x xs) ys)),
; ;;                                      if (< y x)

(define merge (xs ys)
    (if (null? xs)
        ys
        (if (null? ys)
            xs
            (if (< (car xs) (car ys))
                (cons (car xs) (merge (cdr xs) ys))
                (cons (car ys) (merge xs (cdr ys)))))))

        (check-expect (merge '(1 2 3) '()) '(1 2 3))
        (check-expect (merge '() '(1 3 6)) '(1 3 6))
        (check-expect (merge '(2 4 5 7 8 10) '(1 3 6)) '(1 2 3 4 5 6 7 8 10))

; ;; (merge-sort xs) This function will perform a merge-sort on a given list
; ;;                 and will return a list of numbers sorted in increasing
; ;;                 order.

(define merge-sort (xs)
    (if (singleton? xs)
        xs
        (let ([result (split-list xs)])
            (merge (merge-sort (car result)) (merge-sort (cadr result))))))

        (check-expect (merge-sort '(7 5 3 6 1 8 4 2)) '(1 2 3 4 5 6 7 8))
