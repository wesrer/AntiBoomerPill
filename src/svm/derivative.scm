(val N 5000)
(define deriv-aux (a) (list3 '/ (deriv a) a))

(define deriv (a)
  (cond
    ((atom? a) (if (= a 'x) 1 0))
    ((= (car a) '+) (cons '+ (map deriv (cdr a))))
    ((= (car a) '-) (cons '- (map deriv (cdr a))))
    ((= (car a) '*) (list3 '* a (cons '+ (map deriv-aux (cdr a)))))
    ((= (car a) '/) (cons '-
                          (list3
                            '/
                            (deriv (cadr a))(caddr a)
                            (list3 '/
                                  (cadr a)
                                  (cons
                                    (list3 '* (caddr a) (caddr a))
                                    (deriv (caddr a)))
                            )
                           )
                    )
     )
    (#t ’error)
   )
)

(define benchmark (start end)
  (while (< start end)
    (begin
      (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
      (set start (+ start 1))
    ))
)

(benchmark (0 N))


