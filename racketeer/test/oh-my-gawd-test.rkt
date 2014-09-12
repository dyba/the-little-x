(module oh-my-gawd-test racket
  (require rackunit)
  (require (file "../src/oh-my-gawd.rkt"))

  ;; remove-member*
  (check-equal? (remove-member* 'orange '(this orange looks too orange for an orange))
                '(this looks too for an))
  (check-equal? (remove-member* 'one '(one hit wonder was one heck of a deal))
                '(hit wonder was heck of a deal))
  
  (check-equal? (insert-right* 'roast 'chuck '((how much (wood))
                                               could
                                               ((a (wood) chuck))
                                               (((chuck)))
                                               (if (a) ((wood chuck)))
                                               could chuck wood))
                '((how much (wood))
                  could
                  ((a (wood) chuck roast))
                  (((chuck roast)))
                  (if (a) ((wood chuck roast)))
                  could chuck roast wood))

  ;; occur*
  (check-eq? (occur* 'banana '((banana)
                               (split ((((banana ice)))
                                       (creame (banana))
                                       sherbet))
                               (banana)
                               (bread)
                               (banana brandy)))
             5))