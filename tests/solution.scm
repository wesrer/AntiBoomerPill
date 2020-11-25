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

;; (prefix? xs ys) takes two lists of atoms and returns true if xs is the 
;; prefix of ys, where ys is a non-empty list and ys has a longer length 
;;than xs.

;; laws:
;;   (prefix? (cons x xs) (cons y ys)) == (matching-prefix xs ys), 
;;                                                                  when x = y
;;   (prefix? (cons x xs) (cons y ys)) == #f, when x != y
;;   (prefix? '() ys) == ()
;; [optional notes about where laws come from, or difficulty, if any]

(define prefix? (xs ys)
    (if (null? xs)
        #t
        (if (equal? (car xs) (car ys))
            (prefix? (cdr xs) (cdr ys))
            #f)))

        (check-assert (prefix? '(()) '(())))
        (check-assert (prefix? '(1 2) '(1 2 3)))
        (check-assert (not (prefix? '(1 1) '(1 2 3))))

;; (contig-sublist? xs ys) takes two lists of atoms and determines if xs is a 
;;contiguous subsequence of ys. That is, (contig-sublist? xs ys) returns #t 
;; iff there are two other lists front and back, such that ys is equal to 
;; (append (append front x) back).

;; laws:
;;   (contig-sublist? (cons x xs) (cons y ys)) == (|| (prefix (cons x xs) 
;;                      (cons y ys)) (contig-sublist(cons x xs)  ys))
;;   (contig-sublist? xs '()) == #f
;; [optional notes about where laws come from, or difficulty, if any]

(define contig-sublist? (xs ys)
    (if (null? ys)
        #f
        (|| (prefix? xs ys) (contig-sublist? xs (cdr ys)))))   

        (check-assert (contig-sublist? '(a y b) '(x a a a y b b z c)))
        (check-assert (not (contig-sublist? '(a b c) '(x a y b z c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 10

;; (even? x) takes an number and returns true if the number is even, and 
;; false if not.

;; laws:
;;   (even? x) == #t, where (mod x 2) is 0.
;;   (even? x) == #f, where (mod x 2) is 1.

(define even? (x)
     (= (mod x 2) 0))

        (check-assert (not (even? 5)))
        (check-assert (even? 4))

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

        (check-expect (takewhile even? '(2 4 6 7 8)) '(2 4 6))
        (check-expect (takewhile even? '(3 6 9 8)) '())
        (check-expect (takewhile even? '()) '())

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

        (check-expect (dropwhile even? '(2 4 6 7 8 10 12)) '(7 8 10 12))
        (check-expect (dropwhile even? '(7 8 10 12)) '(7 8 10 12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise B


;; (take n xs) expects a natural number and a list of values. It returns the 
;; longest prefix of xs that contains at most n elements.

;; laws:
;;   (take n '()) == '()
;;   (take n xs) == '(), where n <= 0.
;;   (take n (cons x xs)) == (cons x (take (- n 1) xs)), where n > 0.

(define take (n xs)
    (if (null? xs)
        xs
        (if (<= n 0)
            '()
            (cons (car xs) (take (- n 1) (cdr xs))))))

        (check-expect (take 3 '()) '())
        (check-expect (take 0 '(2 4 6 7 8)) '())
        (check-expect (take 3 '(2 4 6 7 8)) '(2 4 6))

;; (drop n xs) takes a natural number and a list of values. It returns the list
;; with at most the first n elements dropped.

;; laws:
;;   (drop n '()) == '()
;;   (drop n (cons x xs)) == (cons x xs), where n <= 0.
;;   (drop n (cons x xs)) == (drop (- n 1) xs), where n > 0.

(define drop (n xs)
    (if (null? xs)
        '()
        (if (<= n 0)
            (cons (car xs)(cdr xs))
            (drop (- n 1) (cdr xs)))))
        
        (check-expect (drop 5 '()) '())
        (check-expect (drop 0 '(2 4 6 7 8)) '(2 4 6 7 8))
        (check-expect (drop 3 '(2 4 6 7 8)) '(7 8))
        (check-expect (drop 6 '(2 4 6 7 8)) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise C

;; (zip xs ys) takes two lists of equal length and returns the list 
;; created when the pair of lists is converted to a list of pairs. 
;; This is done by associating corresponding values in the two lists, 
;; where a pair is a list of length two.

;; laws:
;;   (zip '() '()) == '()
;;   (zip (cons x xs) (cons y ys)) == (cons (list2 x y) (zip xs ys))

(define zip (xs ys)
    (if (null? xs)
        '()
        (cons (list2 (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

        (check-expect (zip '() '()) '())
        (check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))
        (check-expect (zip '((1 2) (3 4)) '((a b) (c d))) 
                                                '(((1 2) (a b)) ((3 4) (c d))))

;; (concatenate-1st-elem ps) takes a list of pairs and returns a list with the 
;; first element of every pair concatenated into a single list. 

;; laws:
;;   (concatenate-1st-elems '()) == '()
;;   (concatenate-1st-elems (cons p ps)) == (cons (alist-first-key ps) 
;;                                            (concatenate-1st-elems (cdr ps)))

(define concatenate-1st-elems (ps)
    (if (null? ps)
        '()
        (cons (alist-first-key ps) (concatenate-1st-elems (cdr ps)))))

        (check-expect (concatenate-1st-elems '()) '())
        (check-expect (concatenate-1st-elems '((1 2) (3 4) (5 6))) '(1 3 5))

;; (concatenate-2nd-elem ps) takes a list of pairs and returns a list with the 
;; second element of every pair concatenated into a single list.

;; laws:
;;   (concatenate-2st-elems '()) == '()
;;   (concatenate-2st-elems (cons p ps)) == (cons (alist-first-attribute ps) 
;;                                            (concatenate-2nd-elems (cdr ps)))

(define concatenate-2nd-elems (ps)
    (if (null? ps)
        '()
        (cons (alist-first-attribute ps) (concatenate-2nd-elems (cdr ps)))))

        (check-expect (concatenate-2nd-elems '()) '())
        (check-expect (concatenate-2nd-elems '((1 2) (3 4) (5 6))) '(2 4 6))

;; (unzip ps) converts a list of pairs to a pair of lists. This is done by 
;; associating the first values in every pair and the second values in every 
;; pair.

(define unzip (ps)
    (let ([p1 (concatenate-1st-elems ps)] [p2 (concatenate-2nd-elems ps)])
        (list2 p1 p2)))

    (check-expect (unzip '()) '(() ()))
    (check-expect (unzip '((I Magnin) (U Thant) (E Coli))) 
                                                '((I U E) (Magnin Thant Coli)))
    (check-expect (unzip '(((I L 3) (Magnin Group)) ((U A) (Thant Finger)) 
                           ((E F) (Fresh Coli))))
                  '(((I L 3) (U A) (E F)) ((Magnin Group) (Thant Finger)
                                           (Fresh Coli))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise LP

;; (singleton? zs) takes a list and returns true if its length is 1.

;; laws:
;;   (singleton? '()) == #f
;;   (singleton? (cons x '())) == #t

(define singleton? (zs)
    (if (null? zs)
        #f
        (null? (cdr zs))))

        (check-assert (not (singleton? '())))
        (check-assert (not (singleton? '(1 2 3))))
        (check-assert (singleton? '(1)))

;; (has-n-elements? xs n) takes a list and a natural number, and returns 
;; true if the length of the list is n.

;; laws:
;;   (has-n-elements? '() 0) == #t
;;   (has-n-elements? '() n) == #f, where n is not 0.
;;   (has-n-elements? xs 0) == #f, where xs is not nil. 
;;   (has-n-elements? (cons x xs) n) == (has-n-elements? xs (- n 1))

(define has-n-elements? (xs n)
    (if (|| (null? xs) (= n 0))
        (&& (null? xs) (= n 0))
        (has-n-elements? (cdr xs) (- n 1))))

        (check-assert (has-n-elements? '() 0))
        (check-assert (not (has-n-elements? '() 5)))
        (check-assert (not (has-n-elements? '(1 2) 0)))
        (check-assert (has-n-elements? '(1 2) 2))

;; (nearly-same-lengths? as bs) When xs and ys are both lists of values, 
;; (nearly-same-lengths? xs ys) tells if the length of xs and the length of ys 
;; differ by at most 1.

;; laws:
;;   (nearly-same-lengths? '() '()) == (|| (singleton? bs) (null? bs))
;;   (nearly-same-lengths? '() bs) == (|| (singleton? bs) (null? bs))
;;   (nearly-same-lengths? as '()) == (|| (singleton? as) (null? as))

(define nearly-same-lengths? (as bs)
    (if (null? as)
        (|| (singleton? bs) (null? bs))
        (if (null? bs)
            (|| (singleton? as) (null? as))
            (nearly-same-lengths? (cdr as) (cdr bs)))))

        (check-assert (nearly-same-lengths? '() '()))
        (check-assert (nearly-same-lengths? '(1 2 3 4 5) '(1 2 3 4 5 6)))
        (check-assert (not (nearly-same-lengths? '(1 2 3 4 5 6 7 8) 
                                                                '(1 2 3 4 5))))             




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise G


;; (member? m ys) takes an s-expression and a list, and returns true if that
;; s-expression is present in the list and false if not.

;; laws:
;;   (member? m '()) == #f
;;   (member? m (cons y ys)) == #f, where m != y
;;   (member? m (cons m ys)) == #t

(define member? (m ys)
    (if (null? ys)
        #f
        (if (equal? m (car ys))
            #t
            (member? m (cdr ys)))))

        (check-assert (member? '1 '(1 2 3 4)))
        (check-assert (not (member? '1 '(2 3 4))))


                                                                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise H

;; (even-index-elements), when given a list of elements, returns a list with
;; the elements in the even-indexed positions in the list. 
;; (Indexing begins at 0)

;; laws:
;;   (even-index-elements '()) == '()
;;   (even-index-elements xs) == xs, where xs is a singleton
;;   (even-index-elements (cons x1 (cons x2 xs))) == 
;;                                           (cons x1 (even-index-elements xs))


(define even-index-elements (xs)
    (if (null? xs)
        '()
        (if (singleton? xs)
            xs
            (cons (car xs) (even-index-elements (cddr xs))))))

        (check-expect (even-index-elements '()) '())
        (check-expect (even-index-elements '(1 2 3 4 5 6)) '(1 3 5))


;; (odd-index-elements), when given a list of elements, returns a list with
;; the elements in the odd-indexed positions in the list. 
;; (Indexing begins at 0)

;; laws:
;;   (even-index-elements '()) == '()
;;   (even-index-elements xs) == xs, where xs is a singleton
;;   (even-index-elements (cons x1 (cons x2 xs))) == 
;;                                           (cons x2 (even-index-elements xs))

(define odd-index-elements (xs)
    (if (nearly-same-lengths? xs '())
        '()
        (cons (cadr xs) (odd-index-elements (cddr xs)))))

        (check-expect (odd-index-elements '()) '())
        (check-expect (odd-index-elements '(1 2 3 4 5 6)) '(2 4 6))
