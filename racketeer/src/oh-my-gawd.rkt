(module oh-my-gawd racket
  (require racklog)
  (require (file "utils.rkt"))
  (require (file "number-games.rkt")) ;; find a way to only import o+
  (provide (all-defined-out))
  
  (module+ test
    (require rackunit))
  
  (define remove-member*
    (lambda (a l)
      (cond
        ((null? l) '())
        ((scheme-atom? (car l))
         (cond
           ((eq? a (car l))
            (remove-member* a
                            (cdr l)))
           (else (cons (car l)
                       (remove-member* a
                                       (cdr l))))))
        (else (cons (remove-member* a (car l))
                    (remove-member* a (cdr l)))))))
  
  (define insert-right*
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((scheme-atom? (car l))
         (cond
           ((eq? old (car l))
            (cons old
                  (cons new
                        (insert-right* new old (cdr l)))))
           (else (cons (car l)
                       (insert-right* new old (cdr l))))))
        (else (cons (insert-right* new old (car l))
                    (insert-right* new old (cdr l)))))))
  
  (module+ test #f
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
                    could chuck roast wood)))
  
  (define occur*
    (lambda (a lst)
      (cond
        ((null? lst) 0)
        ((scheme-atom? (car lst))
         (cond
           ((eq? (car lst) a)
            (add1 (occur* a (cdr lst))))
           (else (occur* a (cdr lst)))))
        (else (o+ (occur* a (car lst))
                  (occur* a (cdr lst))))))))