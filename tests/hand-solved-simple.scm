(val K 
   (cons 
      (lambda ($closure x) 
         (cons 
            (lambda ($closure _) (CAPTURED-IN 0 $closure) x) 
            (cons (CAPTURED-IN 0 $closure)
             '()))
            ) 
      '()))
