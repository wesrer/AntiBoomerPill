(val N 0)

(define tak (x y z b)
(if (and (not (< y x)) (= b 0))
    z
    (tak (tak (- 1 x) y z (- b 1)) (tak (- 1 y) z x (- b 1)) (tak (- 1 z) x y (- b 1)) (- b 1))
))


(print (tak 5 4 3 0))