(let ([$r0 (lambda ($r1 $r2) 
              (let ([$r3 (null? $r2)]) 
                (if $r3 
                   0 
                   (let ([$r3 (let* ([$r3 equal?]
                                     [$r4 $r1]
                                     [$r5 (car $r2)]) 
                                ($r3 $r4 $r5))]) 
                     (if $r3 
                        (let* ([$r3 1]
                               [$r4 (let* ([$r4 count]
                                           [$r5 $r1]
                                           [$r6 (cdr $r2)]) 
                                      ($r4 $r5 $r6))]) 
                          (+ $r3 $r4)) 
                        (let* ([$r3 count]
                               [$r4 $r1]
                               [$r5 (cdr $r2)]) 
                          ($r3 $r4 $r5)))))))]) 
  (set count $r0))
(let ([$r0 (lambda ($r1 $r2) 
              (let ([$r3 (null? $r2)]) 
                (if $r3 
                   0 
                   (let ([$r3 (let* ([$r3 atom?]
                                     [$r4 (car $r2)]) 
                                ($r3 $r4))]) 
                     (if $r3 
                        (let ([$r3 (let* ([$r3 equal?]
                                          [$r4 $r1]
                                          [$r5 (car $r2)]) 
                                     ($r3 $r4 $r5))]) 
                          (if $r3 
                             (let* ([$r3 1]
                                    [$r4 (let* ([$r4 countall]
                                                [$r5 $r1]
                                                [$r6 (cdr $r2)]) 
                                           ($r4 $r5 $r6))]) 
                               (+ $r3 $r4)) 
                             (let* ([$r3 countall]
                                    [$r4 $r1]
                                    [$r5 (cdr $r2)]) 
                               ($r3 $r4 $r5)))) 
                        (let* ([$r3 (let* ([$r3 count]
                                           [$r4 $r1]
                                           [$r5 (car $r2)]) 
                                      ($r3 $r4 $r5))]
                               [$r4 (let* ([$r4 countall]
                                           [$r5 $r1]
                                           [$r6 (cdr $r2)]) 
                                      ($r4 $r5 $r6))]) 
                          (+ $r3 $r4)))))))]) 
  (set countall $r0))
(let ([$r0 (lambda ($r1) 
              (let ([$r2 (null? $r1)]) 
                (if $r2 
                   '() 
                   (let ([$r2 (let* ([$r2 atom?]
                                     [$r3 (car $r1)]) 
                                ($r2 $r3))]) 
                     (if $r2 
                        (let* ([$r2 append]
                               [$r3 (let* ([$r3 mirror]
                                           [$r4 (cdr $r1)]) 
                                      ($r3 $r4))]
                               [$r4 (let* ([$r4 list1]
                                           [$r5 (car $r1)]) 
                                      ($r4 $r5))]) 
                          ($r2 $r3 $r4)) 
                        (let* ([$r2 append]
                               [$r3 (let* ([$r3 mirror]
                                           [$r4 (cdr $r1)]) 
                                      ($r3 $r4))]
                               [$r4 (let* ([$r4 list1]
                                           [$r5 (let* ([$r5 reverse]
                                                       [$r6 (car $r1)]) 
                                                  ($r5 $r6))]) 
                                      ($r4 $r5))]) 
                          ($r2 $r3 $r4)))))))]) 
  (set mirror $r0))
(let ([$r0 (lambda ($r1) 
              (let ([$r2 (null? $r1)]) 
                (if $r2 
                   '() 
                   (let ([$r2 (let* ([$r2 atom?]
                                     [$r3 (car $r1)]) 
                                ($r2 $r3))]) 
                     (if $r2 
                        (let* ([$r2 (car $r1)]
                               [$r3 (let* ([$r3 flatten]
                                           [$r4 (cdr $r1)]) 
                                      ($r3 $r4))]) 
                          (cons $r2 $r3)) 
                        (let* ([$r2 append]
                               [$r3 (let* ([$r3 flatten]
                                           [$r4 (car $r1)]) 
                                      ($r3 $r4))]
                               [$r4 (let* ([$r4 flatten]
                                           [$r5 (cdr $r1)]) 
                                      ($r4 $r5))]) 
                          ($r2 $r3 $r4)))))))]) 
  (set flatten $r0))
(let ([$r0 (lambda ($r1 $r2 $r3) 
              (let ([$r4 (null? $r1)]) 
                (if $r4 
                   $r3 
                   (let ([$r4 (let* ([$r4 equal?]
                                     [$r5 (car $r1)]
                                     [$r6 (car $r2)]) 
                                ($r4 $r5 $r6))]) 
                     (if $r4 
                        (let* ([$r4 find-contig]
                               [$r5 (cdr $r1)]
                               [$r6 (cdr $r2)]
                               [$r7 #t]) 
                          ($r4 $r5 $r6 $r7)) 
                        (if $r3 
                           #f 
                           (let* ([$r4 find-contig]
                                  [$r5 $r1]
                                  [$r6 (cdr $r2)]
                                  [$r7 #f]) 
                             ($r4 $r5 $r6 $r7))))))))]) 
  (set find-contig $r0))
(let ([$r0 (lambda ($r1 $r2) 
              (let ([$r3 (null? $r1)]) 
                (if $r3 
                   #t 
                   (let ([$r3 (null? $r2)]) 
                     (if $r3 
                        #f 
                        (let* ([$r3 find-contig]
                               [$r4 $r1]
                               [$r5 $r2]
                               [$r6 #f]) 
                          ($r3 $r4 $r5 $r6)))))))]) 
  (set contig-sublist? $r0))
(let ([$r0 (lambda ($r1 $r2 $r3) 
              (let ([$r4 (null? $r1)]) 
                (if $r4 
                   $r3 
                   (let ([$r4 (null? $r2)]) 
                     (if $r4 
                        #f 
                        (let ([$r4 (let* ([$r4 equal?]
                                          [$r5 (car $r1)]
                                          [$r6 (car $r2)]) 
                                     ($r4 $r5 $r6))]) 
                          (if $r4 
                             (let* ([$r4 find-subset]
                                    [$r5 (cdr $r1)]
                                    [$r6 (cdr $r2)]
                                    [$r7 #t]) 
                               ($r4 $r5 $r6 $r7)) 
                             (let* ([$r4 find-subset]
                                    [$r5 $r1]
                                    [$r6 (cdr $r2)]
                                    [$r7 $r3]) 
                               ($r4 $r5 $r6 $r7)))))))))]) 
  (set find-subset $r0))
(let ([$r0 (lambda ($r1 $r2) 
              (let* ([$r3 find-subset]
                     [$r4 $r1]
                     [$r5 $r2]
                     [$r6 #f]) 
                ($r3 $r4 $r5 $r6)))]) 
  (set sublist? $r0))
(let ([$r0 (lambda ($r1) 
              (let* ([$r2 (let* ([$r2 mod]
                                 [$r3 $r1]
                                 [$r4 2]) 
                            ($r2 $r3 $r4))]
                     [$r3 0]) 
                (= $r2 $r3)))]) 
  (set even? $r0))
(let ([$r0 (lambda ($r1 $r2) 
              (let ([$r3 (null? $r2)]) 
                (if $r3 
                   $r2 
                   (let ([$r3 (let* ([$r3 $r1]
                                     [$r4 (car $r2)]) 
                                ($r3 $r4))]) 
                     (if $r3 
                        (let* ([$r3 (car $r2)]
                               [$r4 (let* ([$r4 takewhile]
                                           [$r5 $r1]
                                           [$r6 (cdr $r2)]) 
                                      ($r4 $r5 $r6))]) 
                          (cons $r3 $r4)) 
                        '())))))]) 
  (set takewhile $r0))
(let ([$r0 (lambda ($r1 $r2) 
              (let ([$r3 (null? $r2)]) 
                (if $r3 
                   $r2 
                   (let ([$r3 (let* ([$r3 $r1]
                                     [$r4 (car $r2)]) 
                                ($r3 $r4))]) 
                     (if $r3 
                        (let* ([$r3 dropwhile]
                               [$r4 $r1]
                               [$r5 (cdr $r2)]) 
                          ($r3 $r4 $r5)) 
                        $r2)))))]) 
  (set dropwhile $r0))
(let ([$r0 (lambda ($r1 $r2) 
              (let ([$r3 (null? $r1)]) 
                (if $r3 
                   '() 
                   (let* ([$r3 (let* ([$r3 list2]
                                      [$r4 (car $r1)]
                                      [$r5 (car $r2)]) 
                                 ($r3 $r4 $r5))]
                          [$r4 (let* ([$r4 zip]
                                      [$r5 (cdr $r1)]
                                      [$r6 (cdr $r2)]) 
                                 ($r4 $r5 $r6))]) 
                     (cons $r3 $r4)))))]) 
  (set zip $r0))
(let ([$r0 (lambda ($r1 $r2) 
              (let ([$r3 (let ([$r3 (cdr $r2)]) (null? $r3))]) 
                (if $r3 
                   $r2 
                   (let ([$r3 (let* ([$r3 <=]
                                     [$r4 (let* ([$r4 $r1]
                                                 [$r5 (car $r2)]) 
                                            ($r4 $r5))]
                                     [$r5 (let* ([$r5 $r1]
                                                 [$r6 (let* ([$r6 cadr]
                                                             [$r7 $r2]) 
                                                        ($r6 $r7))]) 
                                            ($r5 $r6))]) 
                                ($r3 $r4 $r5))]) 
                     (if $r3 
                        (let* ([$r3 arg-max]
                               [$r4 (cdr $r2)]) 
                          ($r3 $r4)) 
                        (let* ([$r3 arg-max]
                               [$r4 (let* ([$r4 (car $r2)]
                                           [$r5 (let* ([$r5 cddr]
                                                       [$r6 $r2]) 
                                                  ($r5 $r6))]) 
                                      (cons $r4 $r5))]) 
                          ($r3 $r4)))))))]) 
  (set arg-max $r0))
(let ([$r0 (lambda ($r1 $r2) 
              (let ([$r3 (null? $r1)]) 
                (if $r3 
                   (cons $r1 $r2) 
                   (let ([$r3 (null? $r2)]) 
                     (if $r3 
                        $r1 
                        (let ([$r3 (let* ([$r3 <=]
                                          [$r4 (car $r1)]
                                          [$r5 (car $r2)]) 
                                     ($r3 $r4 $r5))]) 
                          (if $r3 
                             (let* ([$r3 (car $r1)]
                                    [$r4 (let* ([$r4 merge]
                                                [$r5 (cdr $r1)]
                                                [$r6 $r2]) 
                                           ($r4 $r5 $r6))]) 
                               (cons $r3 $r4)) 
                             (let* ([$r3 merge]
                                    [$r4 (let ([$r4 (car $r2)]) 
                                           (cons $r4 $r1))]
                                    [$r5 (cdr $r2)]) 
                               ($r3 $r4 $r5)))))))))]) 
  (set merge $r0))
(let ([$r0 (lambda ($r1 $r2) 
              (let ([$r3 (null? $r1)]) 
                (if $r3 
                   #t 
                   (let ([$r3 (let* ([$r3 equal?]
                                     [$r4 (let* ([$r4 count]
                                                 [$r5 (car $r1)]
                                                 [$r6 $r1]) 
                                            ($r4 $r5 $r6))]
                                     [$r5 (let* ([$r5 count]
                                                 [$r6 (car $r1)]
                                                 [$r7 $r2]) 
                                            ($r5 $r6 $r7))]) 
                                ($r3 $r4 $r5))]) 
                     (if $r3 
                        (let* ([$r3 is-permutation?]
                               [$r4 (cdr $r1)]
                               [$r5 $r2]) 
                          ($r3 $r4 $r5)) 
                        #f)))))]) 
  (set is-permutation? $r0))
(let ([$r0 (lambda ($r1 $r2) 
              (let ([$r3 (let* ([$r3 equal?]
                                [$r4 (let* ([$r4 length]
                                            [$r5 $r1]) 
                                       ($r4 $r5))]
                                [$r5 (let* ([$r5 length]
                                            [$r6 $r2]) 
                                       ($r5 $r6))]) 
                           ($r3 $r4 $r5))]) 
                (if $r3 
                   (let* ([$r3 is-permutation?]
                          [$r4 $r1]
                          [$r5 $r2]) 
                     ($r3 $r4 $r5)) 
                   #f)))]) 
  (set permutation? $r0))
(let ([$r0 (lambda ($r1 $r2 $r3) 
              (let ([$r4 (null? $r1)]) 
                (if $r4 
                   (let ([$r5 (let ([$r6 '()]) (cons $r2 $r6))]) 
                     (cons $r3 $r5)) 
                   (let* ([$r4 list-splitter]
                          [$r5 (cdr $r1)]
                          [$r6 (let ([$r6 (car $r1)]) (cons $r6 $r3))]
                          [$r7 $r2]) 
                     ($r4 $r5 $r6 $r7)))))]) 
  (set list-splitter $r0))
(let ([$r0 (lambda ($r1) 
              (let* ([$r2 list-splitter]
                     [$r3 $r1]
                     [$r4 '()]
                     [$r5 '()]) 
                ($r2 $r3 $r4 $r5)))]) 
  (set split-list $r0))
(begin 
   (let ([$r0 (let* ([$r0 procedure?]
                     [$r1 count]) 
                ($r0 $r1))]) 
     (check $r0 '(procedure? count))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let ([$r0 countall]) (function? $r0))]) 
     (check $r0 '(function? countall))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let ([$r0 mirror]) (function? $r0))]) 
     (check $r0 '(function? mirror))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let ([$r0 flatten]) (function? $r0))]) 
     (check $r0 '(function? flatten))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let ([$r0 contig-sublist?]) (function? $r0))]) 
     (check $r0 '(function? contig-sublist?))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let ([$r0 sublist?]) (function? $r0))]) 
     (check $r0 '(function? sublist?))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let ([$r0 takewhile]) (function? $r0))]) 
     (check $r0 '(function? takewhile))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let ([$r0 dropwhile]) (function? $r0))]) 
     (check $r0 '(function? dropwhile))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let ([$r0 zip]) (function? $r0))]) 
     (check $r0 '(function? zip))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let ([$r0 arg-max]) (function? $r0))]) 
     (check $r0 '(function? arg-max))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let ([$r0 merge]) (function? $r0))]) 
     (check $r0 '(function? merge))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let ([$r0 permutation?]) (function? $r0))]) 
     (check $r0 '(function? permutation?))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let ([$r0 split-list]) (function? $r0))]) 
     (check $r0 '(function? split-list))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let* ([$r0 count]
                     [$r1 5]
                     [$r2 (let* ([$r2 1]
                                 [$r3 (let* ([$r3 2]
                                             [$r4 '()]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(count 5 '(1 2)))) 
   (let ([$r0 0]) 
     (expect $r0 '0)))
(begin 
   (let ([$r0 (let* ([$r0 count]
                     [$r1 1]
                     [$r2 (let* ([$r2 1]
                                 [$r3 (let* ([$r3 'a]
                                             [$r4 '()]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(count 1 '(1 a)))) 
   (let ([$r0 1]) 
     (expect $r0 '1)))
(begin 
   (let ([$r0 (let* ([$r0 count]
                     [$r1 1]
                     [$r2 (let* ([$r2 1]
                                 [$r3 (let* ([$r3 (let* ([$r3 'a]
                                                         [$r4 
                                                              (let* ([$r4 
                                                                          1]
                                                                     [$r5 
                                                                          '()]) 
                                                                (cons 
                                                                   $r4 
                                                                   $r5))]) 
                                                    (cons $r3 $r4))]
                                             [$r4 '()]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(count 1 '(1 (a 1))))) 
   (let ([$r0 1]) 
     (expect $r0 '1)))
(begin 
   (let ([$r0 (let* ([$r0 countall]
                     [$r1 'a]
                     [$r2 (let* ([$r2 1]
                                 [$r3 (let* ([$r3 2]
                                             [$r4 (let* ([$r4 3]
                                                         [$r5 '()]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(countall 'a '(1 2 3)))) 
   (let ([$r0 0]) 
     (expect $r0 '0)))
(begin 
   (let ([$r0 (let* ([$r0 countall]
                     [$r1 'a]
                     [$r2 (let* ([$r2 1]
                                 [$r3 (let* ([$r3 'a]
                                             [$r4 (let* ([$r4 3]
                                                         [$r5 '()]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(countall 'a '(1 a 3)))) 
   (let ([$r0 1]) 
     (expect $r0 '1)))
(begin 
   (let ([$r0 (let* ([$r0 countall]
                     [$r1 'a]
                     [$r2 (let* ([$r2 (let* ([$r2 'a]
                                             [$r3 (let* ([$r3 1]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]
                                 [$r3 (let* ([$r3 3]
                                             [$r4 '()]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(countall 'a '((a 1) 3)))) 
   (let ([$r0 1]) 
     (expect $r0 '1)))
(begin 
   (let ([$r0 (let* ([$r0 countall]
                     [$r1 'a]
                     [$r2 (let* ([$r2 (let* ([$r2 'a]
                                             [$r3 (let* ([$r3 1]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]
                                 [$r3 (let* ([$r3 (let* ([$r3 'a]
                                                         [$r4 
                                                              (let* ([$r4 
                                                                          2]
                                                                     [$r5 
                                                                          '()]) 
                                                                (cons 
                                                                   $r4 
                                                                   $r5))]) 
                                                    (cons $r3 $r4))]
                                             [$r4 (let* ([$r4 3]
                                                         [$r5 '()]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(countall 'a '((a 1) (a 2) 3)))) 
   (let ([$r0 2]) 
     (expect $r0 '2)))
(begin 
   (let ([$r0 (let* ([$r0 countall]
                     [$r1 'a]
                     [$r2 (let* ([$r2 (let* ([$r2 'a]
                                             [$r3 (let* ([$r3 1]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]
                                 [$r3 (let* ([$r3 (let* ([$r3 'a]
                                                         [$r4 
                                                              (let* ([$r4 
                                                                          'a]
                                                                     [$r5 
                                                                          (let* ([$r5 
                                                                                      2]
                                                                                 [$r6 
                                                                                      '()]) 
                                                                            (cons 
                                                                               $r5 
                                                                               $r6))]) 
                                                                (cons 
                                                                   $r4 
                                                                   $r5))]) 
                                                    (cons $r3 $r4))]
                                             [$r4 (let* ([$r4 3]
                                                         [$r5 '()]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(countall 'a '((a 1) (a a 2) 3)))) 
   (let ([$r0 3]) 
     (expect $r0 '3)))
(begin 
   (let ([$r0 (let* ([$r0 mirror]
                     [$r1 '()]) 
                ($r0 $r1))]) 
     (check $r0 '(mirror '()))) 
   (let ([$r0 '()]) 
     (expect $r0 ''())))
(begin 
   (let ([$r0 (let* ([$r0 mirror]
                     [$r1 (let* ([$r1 3]
                                 [$r2 (let* ([$r2 2]
                                             [$r3 (let* ([$r3 1]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]) 
                ($r0 $r1))]) 
     (check $r0 '(mirror '(3 2 1)))) 
   (let ([$r0 (let* ([$r0 1]
                     [$r1 (let* ([$r1 2]
                                 [$r2 (let* ([$r2 3]
                                             [$r3 '()]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]) 
                (cons $r0 $r1))]) 
     (expect $r0 ''(1 2 3))))
(begin 
   (let ([$r0 (let* ([$r0 mirror]
                     [$r1 (let* ([$r1 3]
                                 [$r2 (let* ([$r2 2]
                                             [$r3 (let* ([$r3 
                                                              (let* ([$r3 
                                                                          1]
                                                                     [$r4 
                                                                          (let* ([$r4 
                                                                                      4]
                                                                                 [$r5 
                                                                                      '()]) 
                                                                            (cons 
                                                                               $r4 
                                                                               $r5))]) 
                                                                (cons 
                                                                   $r3 
                                                                   $r4))]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]) 
                ($r0 $r1))]) 
     (check $r0 '(mirror '(3 2 (1 4))))) 
   (let ([$r0 (let* ([$r0 (let* ([$r0 4]
                                 [$r1 (let* ([$r1 1]
                                             [$r2 '()]) 
                                        (cons $r1 $r2))]) 
                            (cons $r0 $r1))]
                     [$r1 (let* ([$r1 2]
                                 [$r2 (let* ([$r2 3]
                                             [$r3 '()]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]) 
                (cons $r0 $r1))]) 
     (expect $r0 ''((4 1) 2 3))))
(begin 
   (let ([$r0 (let* ([$r0 flatten]
                     [$r1 '()]) 
                ($r0 $r1))]) 
     (check $r0 '(flatten '()))) 
   (let ([$r0 '()]) 
     (expect $r0 ''())))
(begin 
   (let ([$r0 (let* ([$r0 flatten]
                     [$r1 (let* ([$r1 (let* ([$r1 'a]
                                             [$r2 '()]) 
                                        (cons $r1 $r2))]
                                 [$r2 (let* ([$r2 (let* ([$r2 
                                                              (let* ([$r2 
                                                                          'b]
                                                                     [$r3 
                                                                          '()]) 
                                                                (cons 
                                                                   $r2 
                                                                   $r3))]
                                                         [$r3 '()]) 
                                                    (cons $r2 $r3))]
                                             [$r3 '()]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]) 
                ($r0 $r1))]) 
     (check $r0 '(flatten '((a) ((b)))))) 
   (let ([$r0 (let* ([$r0 'a]
                     [$r1 (let* ([$r1 'b]
                                 [$r2 '()]) 
                            (cons $r1 $r2))]) 
                (cons $r0 $r1))]) 
     (expect $r0 ''(a b))))
(begin 
   (let ([$r0 (let* ([$r0 contig-sublist?]
                     [$r1 (let* ([$r1 'a]
                                 [$r2 (let* ([$r2 'b]
                                             [$r3 (let* ([$r3 'c]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]
                     [$r2 (let* ([$r2 'x]
                                 [$r3 (let* ([$r3 'y]
                                             [$r4 (let* ([$r4 'a]
                                                         [$r5 
                                                              (let* ([$r5 
                                                                          'b]
                                                                     [$r6 
                                                                          (let* ([$r6 
                                                                                      'c]
                                                                                 [$r7 
                                                                                      (let* ([$r7 
                                                                                                  'm]
                                                                                             [$r8 
                                                                                                  (let* ([$r8 
                                                                                                              'n]
                                                                                                         [$r9 
                                                                                                              '()]) 
                                                                                                    (cons 
                                                                                                       $r8 
                                                                                                       $r9))]) 
                                                                                        (cons 
                                                                                           $r7 
                                                                                           $r8))]) 
                                                                            (cons 
                                                                               $r6 
                                                                               $r7))]) 
                                                                (cons 
                                                                   $r5 
                                                                   $r6))]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(contig-sublist? '(a b c) '(x y a b c m n)))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let* ([$r0 contig-sublist?]
                     [$r1 (let* ([$r1 'a]
                                 [$r2 (let* ([$r2 'b]
                                             [$r3 (let* ([$r3 'c]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]
                     [$r2 (let* ([$r2 'x]
                                 [$r3 (let* ([$r3 'f]
                                             [$r4 (let* ([$r4 'a]
                                                         [$r5 
                                                              (let* ([$r5 
                                                                          'y]
                                                                     [$r6 
                                                                          (let* ([$r6 
                                                                                      'b]
                                                                                 [$r7 
                                                                                      (let* ([$r7 
                                                                                                  'c]
                                                                                             [$r8 
                                                                                                  (let* ([$r8 
                                                                                                              'm]
                                                                                                         [$r9 
                                                                                                              (let* ([$r9 
                                                                                                                          'n]
                                                                                                                     [$r10 
                                                                                                                           '()]) 
                                                                                                                (cons 
                                                                                                                   $r9 
                                                                                                                   $r10))]) 
                                                                                                    (cons 
                                                                                                       $r8 
                                                                                                       $r9))]) 
                                                                                        (cons 
                                                                                           $r7 
                                                                                           $r8))]) 
                                                                            (cons 
                                                                               $r6 
                                                                               $r7))]) 
                                                                (cons 
                                                                   $r5 
                                                                   $r6))]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(contig-sublist? '(a b c) '(x f a y b c m n)))) 
   (let ([$r0 #f]) 
     (expect $r0 '#f)))
(begin 
   (let ([$r0 (let* ([$r0 sublist?]
                     [$r1 (let* ([$r1 'a]
                                 [$r2 (let* ([$r2 'b]
                                             [$r3 (let* ([$r3 'c]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]
                     [$r2 (let* ([$r2 'x]
                                 [$r3 (let* ([$r3 'y]
                                             [$r4 (let* ([$r4 'a]
                                                         [$r5 
                                                              (let* ([$r5 
                                                                          'z]
                                                                     [$r6 
                                                                          (let* ([$r6 
                                                                                      'b]
                                                                                 [$r7 
                                                                                      (let* ([$r7 
                                                                                                  'c]
                                                                                             [$r8 
                                                                                                  (let* ([$r8 
                                                                                                              'm]
                                                                                                         [$r9 
                                                                                                              (let* ([$r9 
                                                                                                                          'n]
                                                                                                                     [$r10 
                                                                                                                           '()]) 
                                                                                                                (cons 
                                                                                                                   $r9 
                                                                                                                   $r10))]) 
                                                                                                    (cons 
                                                                                                       $r8 
                                                                                                       $r9))]) 
                                                                                        (cons 
                                                                                           $r7 
                                                                                           $r8))]) 
                                                                            (cons 
                                                                               $r6 
                                                                               $r7))]) 
                                                                (cons 
                                                                   $r5 
                                                                   $r6))]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(sublist? '(a b c) '(x y a z b c m n)))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let* ([$r0 sublist?]
                     [$r1 (let* ([$r1 'a]
                                 [$r2 (let* ([$r2 'b]
                                             [$r3 (let* ([$r3 'c]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]
                     [$r2 (let* ([$r2 'x]
                                 [$r3 (let* ([$r3 'f]
                                             [$r4 (let* ([$r4 'a]
                                                         [$r5 
                                                              (let* ([$r5 
                                                                          'y]
                                                                     [$r6 
                                                                          (let* ([$r6 
                                                                                      'b]
                                                                                 [$r7 
                                                                                      (let* ([$r7 
                                                                                                  'm]
                                                                                             [$r8 
                                                                                                  (let* ([$r8 
                                                                                                              'n]
                                                                                                         [$r9 
                                                                                                              '()]) 
                                                                                                    (cons 
                                                                                                       $r8 
                                                                                                       $r9))]) 
                                                                                        (cons 
                                                                                           $r7 
                                                                                           $r8))]) 
                                                                            (cons 
                                                                               $r6 
                                                                               $r7))]) 
                                                                (cons 
                                                                   $r5 
                                                                   $r6))]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(sublist? '(a b c) '(x f a y b m n)))) 
   (let ([$r0 #f]) 
     (expect $r0 '#f)))
(begin 
   (let ([$r0 (let* ([$r0 takewhile]
                     [$r1 even?]
                     [$r2 (let* ([$r2 2]
                                 [$r3 (let* ([$r3 4]
                                             [$r4 (let* ([$r4 6]
                                                         [$r5 
                                                              (let* ([$r5 
                                                                          7]
                                                                     [$r6 
                                                                          (let* ([$r6 
                                                                                      8]
                                                                                 [$r7 
                                                                                      (let* ([$r7 
                                                                                                  10]
                                                                                             [$r8 
                                                                                                  (let* ([$r8 
                                                                                                              12]
                                                                                                         [$r9 
                                                                                                              '()]) 
                                                                                                    (cons 
                                                                                                       $r8 
                                                                                                       $r9))]) 
                                                                                        (cons 
                                                                                           $r7 
                                                                                           $r8))]) 
                                                                            (cons 
                                                                               $r6 
                                                                               $r7))]) 
                                                                (cons 
                                                                   $r5 
                                                                   $r6))]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(takewhile even? '(2 4 6 7 8 10 12)))) 
   (let ([$r0 (let* ([$r0 2]
                     [$r1 (let* ([$r1 4]
                                 [$r2 (let* ([$r2 6]
                                             [$r3 '()]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]) 
                (cons $r0 $r1))]) 
     (expect $r0 ''(2 4 6))))
(begin 
   (let ([$r0 (let* ([$r0 dropwhile]
                     [$r1 even?]
                     [$r2 (let* ([$r2 2]
                                 [$r3 (let* ([$r3 4]
                                             [$r4 (let* ([$r4 6]
                                                         [$r5 
                                                              (let* ([$r5 
                                                                          7]
                                                                     [$r6 
                                                                          (let* ([$r6 
                                                                                      8]
                                                                                 [$r7 
                                                                                      (let* ([$r7 
                                                                                                  10]
                                                                                             [$r8 
                                                                                                  (let* ([$r8 
                                                                                                              12]
                                                                                                         [$r9 
                                                                                                              '()]) 
                                                                                                    (cons 
                                                                                                       $r8 
                                                                                                       $r9))]) 
                                                                                        (cons 
                                                                                           $r7 
                                                                                           $r8))]) 
                                                                            (cons 
                                                                               $r6 
                                                                               $r7))]) 
                                                                (cons 
                                                                   $r5 
                                                                   $r6))]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(dropwhile even? '(2 4 6 7 8 10 12)))) 
   (let ([$r0 (let* ([$r0 7]
                     [$r1 (let* ([$r1 8]
                                 [$r2 (let* ([$r2 10]
                                             [$r3 (let* ([$r3 12]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]) 
                (cons $r0 $r1))]) 
     (expect $r0 ''(7 8 10 12))))
(begin 
   (let ([$r0 (let* ([$r0 zip]
                     [$r1 (let* ([$r1 1]
                                 [$r2 (let* ([$r2 2]
                                             [$r3 (let* ([$r3 3]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]
                     [$r2 (let* ([$r2 'a]
                                 [$r3 (let* ([$r3 'b]
                                             [$r4 (let* ([$r4 'c]
                                                         [$r5 '()]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(zip '(1 2 3) '(a b c)))) 
   (let ([$r0 (let* ([$r0 (let* ([$r0 1]
                                 [$r1 (let* ([$r1 'a]
                                             [$r2 '()]) 
                                        (cons $r1 $r2))]) 
                            (cons $r0 $r1))]
                     [$r1 (let* ([$r1 (let* ([$r1 2]
                                             [$r2 (let* ([$r2 'b]
                                                         [$r3 '()]) 
                                                    (cons $r2 $r3))]) 
                                        (cons $r1 $r2))]
                                 [$r2 (let* ([$r2 (let* ([$r2 3]
                                                         [$r3 
                                                              (let* ([$r3 
                                                                          'c]
                                                                     [$r4 
                                                                          '()]) 
                                                                (cons 
                                                                   $r3 
                                                                   $r4))]) 
                                                    (cons $r2 $r3))]
                                             [$r3 '()]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]) 
                (cons $r0 $r1))]) 
     (expect $r0 ''((1 a) (2 b) (3 c)))))
(begin 
   (let ([$r0 (let* ([$r0 merge]
                     [$r1 (let* ([$r1 4]
                                 [$r2 (let* ([$r2 7]
                                             [$r3 (let* ([$r3 9]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]
                     [$r2 (let* ([$r2 5]
                                 [$r3 (let* ([$r3 6]
                                             [$r4 (let* ([$r4 8]
                                                         [$r5 '()]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(merge '(4 7 9) '(5 6 8)))) 
   (let ([$r0 (let* ([$r0 4]
                     [$r1 (let* ([$r1 5]
                                 [$r2 (let* ([$r2 6]
                                             [$r3 (let* ([$r3 7]
                                                         [$r4 
                                                              (let* ([$r4 
                                                                          8]
                                                                     [$r5 
                                                                          (let* ([$r5 
                                                                                      9]
                                                                                 [$r6 
                                                                                      '()]) 
                                                                            (cons 
                                                                               $r5 
                                                                               $r6))]) 
                                                                (cons 
                                                                   $r4 
                                                                   $r5))]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]) 
                (cons $r0 $r1))]) 
     (expect $r0 ''(4 5 6 7 8 9))))
(begin 
   (let ([$r0 (let* ([$r0 merge]
                     [$r1 (let* ([$r1 4]
                                 [$r2 (let* ([$r2 5]
                                             [$r3 '()]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]
                     [$r2 '()]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(merge '(4 5) '()))) 
   (let ([$r0 (let* ([$r0 4]
                     [$r1 (let* ([$r1 5]
                                 [$r2 '()]) 
                            (cons $r1 $r2))]) 
                (cons $r0 $r1))]) 
     (expect $r0 ''(4 5))))
(begin 
   (let ([$r0 (let* ([$r0 permutation?]
                     [$r1 (let* ([$r1 'a]
                                 [$r2 (let* ([$r2 'b]
                                             [$r3 (let* ([$r3 'c]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]
                     [$r2 (let* ([$r2 'c]
                                 [$r3 (let* ([$r3 'a]
                                             [$r4 (let* ([$r4 'b]
                                                         [$r5 '()]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(permutation? '(a b c) '(c a b)))) 
   (let ([$r0 #t]) 
     (expect $r0 '#t)))
(begin 
   (let ([$r0 (let* ([$r0 permutation?]
                     [$r1 (let* ([$r1 'a]
                                 [$r2 (let* ([$r2 'b]
                                             [$r3 (let* ([$r3 'c]
                                                         [$r4 
                                                              (let* ([$r4 
                                                                          'd]
                                                                     [$r5 
                                                                          '()]) 
                                                                (cons 
                                                                   $r4 
                                                                   $r5))]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]
                     [$r2 (let* ([$r2 'c]
                                 [$r3 (let* ([$r3 'a]
                                             [$r4 (let* ([$r4 'b]
                                                         [$r5 '()]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(permutation? '(a b c d) '(c a b)))) 
   (let ([$r0 #f]) 
     (expect $r0 '#f)))
(begin 
   (let ([$r0 (let* ([$r0 permutation?]
                     [$r1 (let* ([$r1 'a]
                                 [$r2 (let* ([$r2 'b]
                                             [$r3 (let* ([$r3 'a]
                                                         [$r4 '()]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]
                     [$r2 (let* ([$r2 'b]
                                 [$r3 (let* ([$r3 'a]
                                             [$r4 (let* ([$r4 'b]
                                                         [$r5 '()]) 
                                                    (cons $r4 $r5))]) 
                                        (cons $r3 $r4))]) 
                            (cons $r2 $r3))]) 
                ($r0 $r1 $r2))]) 
     (check $r0 '(permutation? '(a b a) '(b a b)))) 
   (let ([$r0 #f]) 
     (expect $r0 '#f)))
(begin 
   (let ([$r0 (let* ([$r0 split-list]
                     [$r1 (let* ([$r1 'a]
                                 [$r2 (let* ([$r2 'b]
                                             [$r3 (let* ([$r3 'c]
                                                         [$r4 
                                                              (let* ([$r4 
                                                                          'd]
                                                                     [$r5 
                                                                          '()]) 
                                                                (cons 
                                                                   $r4 
                                                                   $r5))]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]) 
                ($r0 $r1))]) 
     (check $r0 '(split-list '(a b c d)))) 
   (let ([$r0 (let* ([$r0 (let* ([$r0 'c]
                                 [$r1 (let* ([$r1 'a]
                                             [$r2 '()]) 
                                        (cons $r1 $r2))]) 
                            (cons $r0 $r1))]
                     [$r1 (let* ([$r1 (let* ([$r1 'd]
                                             [$r2 (let* ([$r2 'b]
                                                         [$r3 '()]) 
                                                    (cons $r2 $r3))]) 
                                        (cons $r1 $r2))]
                                 [$r2 '()]) 
                            (cons $r1 $r2))]) 
                (cons $r0 $r1))]) 
     (expect $r0 ''((c a) (d b)))))
(begin 
   (let ([$r0 (let* ([$r0 split-list]
                     [$r1 (let* ([$r1 'a]
                                 [$r2 (let* ([$r2 'b]
                                             [$r3 (let* ([$r3 'c]
                                                         [$r4 
                                                              (let* ([$r4 
                                                                          'd]
                                                                     [$r5 
                                                                          (let* ([$r5 
                                                                                      'e]
                                                                                 [$r6 
                                                                                      '()]) 
                                                                            (cons 
                                                                               $r5 
                                                                               $r6))]) 
                                                                (cons 
                                                                   $r4 
                                                                   $r5))]) 
                                                    (cons $r3 $r4))]) 
                                        (cons $r2 $r3))]) 
                            (cons $r1 $r2))]) 
                ($r0 $r1))]) 
     (check $r0 '(split-list '(a b c d e)))) 
   (let ([$r0 (let* ([$r0 (let* ([$r0 'd]
                                 [$r1 (let* ([$r1 'b]
                                             [$r2 '()]) 
                                        (cons $r1 $r2))]) 
                            (cons $r0 $r1))]
                     [$r1 (let* ([$r1 (let* ([$r1 'e]
                                             [$r2 (let* ([$r2 'c]
                                                         [$r3 
                                                              (let* ([$r3 
                                                                          'a]
                                                                     [$r4 
                                                                          '()]) 
                                                                (cons 
                                                                   $r3 
                                                                   $r4))]) 
                                                    (cons $r2 $r3))]) 
                                        (cons $r1 $r2))]
                                 [$r2 '()]) 
                            (cons $r1 $r2))]) 
                (cons $r0 $r1))]) 
     (expect $r0 ''((d b) (e c a)))))
