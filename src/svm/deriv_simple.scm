(val N 50)

(define deriv-aux (a) (list3 '/ (deriv a) a))

(define deriv (a)
  (cond
    [(atom? a) (if (= a 'x) 1 0)]
    [#t ’error]
   )
)

(define benchmark (start end)
  (while (< start end)
    (begin
      (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
      (set start (+ start 1))
    ))
)

(benchmark 0 N)


