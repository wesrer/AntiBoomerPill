(define simp_rec (m) (if (> m 0) (simp_rec (- m 1)) m))

(check-expect (simp_rec 7) 0)
