(module number-games racket
  (require rackunit)
  
  (provide (all-defined-out))
  
  (define add1
    (lambda (n)
      (+ n 1)))
  
  (define sub1
    (lambda (n)
      (- n 1)))
  
  (define o+
    (lambda (n m)
      (cond
        ((zero? m) n)
        (else (add1 (o+ n (sub1 m)))))))
  
  (define o-
    (lambda (n m)
      (cond
        ((zero? m) n)
        (else (sub1 (o- n (sub1 m)))))))
  
  (define addtup
    (lambda (tup)
      (cond
        ((null? tup) 0)
        (else (o+ (car tup) (addtup (cdr tup)))))))
  
  (define o*
    (lambda (n m)
      (cond
        ((zero? m) 0)
        (else (o+ n (o* n (sub1 m)))))))
  
  (define tup+
    (lambda (tup1 tup2)
      (cond
        ((null? tup1) tup2)
        ((null? tup2) tup1)
        (else (cons (o+ (car tup1) (car tup2))
                    (tup+ (cdr tup1) (cdr tup2)))))))
  
  (define o>
    (lambda (n m)
      (cond
        ((zero? n) false)
        ((zero? m) true)
        (else (o> (sub1 n) (sub1 m))))))
  
  (define o<
    (lambda (n m)
      (cond
        ((zero? m) false)
        ((zero? n) true)
        (else (o< (sub1 n) (sub1 m))))))
  
  (define o=
    (lambda (n m)
      (cond
        ((o> n m) false)
        ((o< n m) false)
        (else true))))
  
  (define o^
    (lambda (n m)
      (cond
        ((zero? m) 1)
        (else (o* n (o^ n (sub1 m)))))))
  
  (define o/
    (lambda (n m)
      (cond
        ((o< n m) 0)
        (else (add1 (o/ (o- n m) m))))))
  
  (define length
    (lambda (lat)
      (cond
        ((null? lat) 0)
        (else (add1 (length (cdr lat)))))))
  
  (define pick
    (lambda (n lat)
      (cond
        ((zero? (sub1 n)) (car lat))
        (else (pick (sub1 n) (cdr lat))))))
  
  (define remove-pick
    (lambda (n lat)
      (cond
        ((zero? (sub1 n)) (cdr lat))
        (else (cons (car lat)
                    (remove-pick (sub1 n)
                                 (cdr lat)))))))  
  
  (define no-nums
    (lambda (lat)
      (cond
        ((null? lat) '())
        ((number? (car lat)) (no-nums (cdr lat)))
        (else (cons (car lat)
                    (no-nums (cdr lat)))))))
    
  (define all-nums
    (lambda (lat)
      (cond
        ((null? lat) '())
        ((number? (car lat)) (cons (car lat)
                                   (all-nums (cdr lat))))
        (else (all-nums (cdr lat))))))
  
  (define eqan?
    (lambda (a1 a2)
      (cond
        ((and (number? a1) (number? a2)) (o= a1 a2))
        ((or (number? a1) (number? a2)) false)
        (else (equal? a1 a2)))))
  
  (define occur
    (lambda (a lst)
      (cond
        ((null? lst) 0)
        ((eq? a (car lst)) (add1 (occur a (cdr lst))))
        (else (occur a (cdr lst))))))
  
  (define one?
    (lambda (n)
      (o= n 1)))
  
  (define remove-pick-v2
    (lambda (n lat)
      (cond
        ((one? n) (cdr lat))
        (else (cons (car lat)
                    (remove-pick-v2 (sub1 n)
                                    (cdr lat))))))))