(val N 32000)

(define deriv-aux (a) (list3 '/ (deriv a) a))

(define deriv (a)
  (if (atom? a) 
      (if (= a 'x) 1 0)
      (if (= (car a) '+) 
          (cons '+ (map deriv (cdr a)))
          (if (= (car a) '-)
              (cons '- (map deriv (cdr a)))
              (if (= (car a) '*) 
                  (list3 '* a (cons '+ (map deriv-aux (cdr a))))
                  (if (= (car a) '/)
                      (list3 '-
                             (list3 '/ 
                                    (deriv (cadr a))
                                    (caddr a))
                             (list3 '/
                                    (cadr a)
                                    (list4 ('* (caddr a) (caddr a)(deriv (caddr a))))))
                       'error)))))
)

(define benchmark (start end)
  (while (< start end)
    (begin
      (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
      (set start (+ start 1))
    ))
)

(benchmark 0 N)
(print N)


