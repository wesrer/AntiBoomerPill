(val N 200)  ;; median of 3 runs == 1.05s

;; tak, gabriel benchmarks, page 82


(define tak (x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define run-tak (k)
  (if (= k 0)
      #t
      (begin
        (tak 18 12 6)
        (run-tak (- k 1)))))

(check-expect (tak 18 12 6) 7)
(check-assert (run-tak N))


