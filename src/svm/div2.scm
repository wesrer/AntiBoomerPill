(val N 2250)

(define cddr (sx) (cdr (cdr  sx)))
(define not (b)   (if b #f #t))

(define create-n (n)
  (let ([a '()])
    (begin 
      (while (!= n 0)
        (begin 
          (set a (cons '() a))
          (set n (- n 1))))
      a)))

(val ll (create-n (* N 2)))

(define iterative-div2 (l)
  (let ([a '()])
    (begin 
      (while (not (null? l))
        (begin 
          (set a (cons (car l) a))
          (set l (cddr l))))
      a)))

(define recursive-div2 (l)
  (if (null? l)
    '()
    (cons (car l) (recursive-div2 (cddr l)))))

(define test-1 (l)
  (let ([i 300])
    (while (> i 0)
      (begin 
        (iterative-div2 l)
        (iterative-div2 l)
        (iterative-div2 l)
        (iterative-div2 l)
        (set i (- i 1))))))

(define test-2 (l)
  (let ([i 300])
    (while (> i 0)
      (begin 
        (recursive-div2 l)
        (recursive-div2 l)
        (recursive-div2 l)
        (recursive-div2 l)
        (set i (- i 1))))))

(define testdiv2-iter ()
  (test-1 ll))

(define testdiv2-recur ()
  (test-2 ll))

(define testdiv2 ()
  (begin 
    (testdiv2-iter)
    (testdiv2-recur)))

(testdiv2)