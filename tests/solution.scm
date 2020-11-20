
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
