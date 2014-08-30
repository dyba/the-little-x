(module number-games-test racket
  (require rackunit)
  (require (file "../src/number-games.rkt"))

  ;; o-
  (check-eq? (o- 3 2) 1)
  
  ;; o+
  (check-eq? (o+ 1 3) 4)
  
  ;; addtup
  (check-eq? (addtup '(1 2 3)) 6)
  (check-eq? (addtup '(10 20 35)) 65)
  
  ;; o*
  (check-eq? (o* 3 1) 3)
  (check-eq? (o* 6 3) 18)
  (check-eq? (o* 10 15) 150)
  
  ;; tup+
  (check-equal? (tup+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))
  (check-equal? (tup+ '(2 3) '(4 6)) '(6 9))
  (check-equal? (tup+ '(3 7) '(4 6 8 1)) '(7 13 8 1))
  (check-equal? (tup+ '(3 7 8 1) '(4 6)) '(7 13 8 1))
  
  ;; o>
  (check-eq? (o> 3 1) true)
  (check-eq? (o> 5 10) false)
  (check-eq? (o> 0 3) false)
  (check-eq? (o> 0 0) false)
  (check-eq? (o> 3 0) true)
  
  ;; o<
  (check-eq? (o< 3 1) false)
  (check-eq? (o< 5 10) true)
  (check-eq? (o< 0 3) true)
  (check-eq? (o< 0 0) false)
  (check-eq? (o< 3 0) false)
  
  ;; o=
  (check-eq? (o= 3 1) false)
  (check-eq? (o= 3 3) true)
  
  ;; o^
  (check-eq? (o^ 3 2) 9)
  (check-eq? (o^ 3 3) 27)
  
  ;; o/
  (check-eq? (o/ 9 3) 3)
  (check-eq? (o/ 10 4) 2)
  (check-eq? (o/ 15 2) 7)
  
  ;; length
  (check-eq? (length '(1 2 3 4 5)) 5)
  (check-eq? (length '(5 5 5)) 3)
  
  ;; pick
  (check-equal? (pick 1 '(sea food)) 'sea)
  (check-equal? (pick 3 '(hot dogs with mustard)) 'with)
  
  ;; remove-pick
  (check-equal? (remove-pick 3 '(hotdogs with hot mustard)) '(hotdogs with mustard))
  
  ;; no-nums
  (check-equal? (no-nums '(5 pears 6 prunes 9 dates)) '(pears prunes dates))

  ;; all-nums
  (check-equal? (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9)))
