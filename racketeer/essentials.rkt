#lang racket

(require rackunit)

(define atom?
  (lambda (atom)
    (and (not (null? atom))
         (not (pair? atom)))))

(define list-of-atoms?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((atom? (car lat)) (list-of-atoms? (cdr lat)))
      (else #f))))

(check-eq? (list-of-atoms? '(one two three)) #t)
(check-eq? (list-of-atoms? '((one) (two) three)) #f)
(check-eq? (list-of-atoms? '()) #t)
(check-eq? (list-of-atoms? '(one (two) three)) #f)

(define member?
  (lambda (atom lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) atom) #t)
      (else (member? atom (cdr lat))))))

(check-eq? (member? 'two '(one two)) #t)
(check-eq? (member? 'three '(one two)) #f)
(check-eq? (member? 'one '()) #f)
(check-eq? (member? 'one '(two one three)) #t)
(check-eq? (member? 'tea '(coffee tea or milk)) #t)
(check-eq? (member? 'poached '(fried eggs and scrambled eggs)) #f)

(define remove-member
  (lambda (atom lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) atom) (cdr lat))
      (else (cons (car lat)
                  (remove-member atom (cdr lat)))))))

(check-equal? (remove-member 'two '(one two three)) '(one three))
(check-equal? (remove-member 'three '(one two three)) '(one two))
(check-equal? (remove-member 'four '(one two three)) '(one two three))

(define firsts
  (lambda (llat)
    (cond
      ((null? llat) '())
      ((atom? (car llat)) (car llat))
      (else (cons (firsts (car llat))
                  (firsts (cdr llat)))))))

(check-equal? (firsts '((five plums) (four) (eleven green oranges))) '(five four eleven))
(check-equal? (firsts '((one) (two) (three))) '(one two three))

(define insert-right
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old
                                 (cons new (cdr lat))))
      (else (cons (car lat)
                  (insert-right new old (cdr lat)))))))

(check-equal? (insert-right 'three 'two '(one two four)) '(one two three four))
(check-equal? (insert-right 'e 'b '(a b c d e)) '(a b e c d e))

(define insert-left
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat)
                  (insert-left new old (cdr lat)))))))

(check-equal? (insert-left 'three 'two '(one two four)) '(one three two four))
(check-equal? (insert-left 'e 'b '(a b c d e)) '(a e b c d e))

(define substitute
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat)
                  (substitute new old (cdr lat)))))))

(check-equal? (substitute 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping for dessert))
(check-equal? (substitute 'hello 'world '(hello to my planet))
              '(hello to my planet))

(define substitute-2
  (lambda (new old1 old2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) old1)
           (eq? (car lat) old2)) (cons new (cdr lat)))
      (else (cons (car lat)
                  (substitute-2 new old1 old2 (cdr lat)))))))

(check-equal? (substitute-2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
              '(vanilla ice cream with chocolate topping))
(check-equal? (substitute-2 'vanilla 'banana 'chocolate '(banana ice cream with chocolate topping))
              '(vanilla ice cream with chocolate topping))
(check-equal? (substitute-2 'vanilla 'chocolate 'topping '(banana ice cream with chocolate topping))
              '(banana ice cream with vanilla topping))

(define multi-remove-member
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multi-remove-member a (cdr lat)))
      (else (cons (car lat)
                  (multi-remove-member a (cdr lat)))))))

(check-equal? (multi-remove-member 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea and hick))
(check-equal? (multi-remove-member 'and '(coffee and cereal and bread))
              '(coffee cereal bread))

(define multi-insert-right
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old
                                 (cons new
                                       (multi-insert-right new old (cdr lat)))))
      (else (cons (car lat)
                  (multi-insert-right new old (cdr lat)))))))

(check-equal? (multi-insert-right 'some 'and '(coffee and cereal and bread))
              '(coffee and some cereal and some bread))

(define multi-insert-left
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new
                                 (cons old
                                       (multi-insert-left new old (cdr lat)))))
      (else (cons (car lat)
                  (multi-insert-left new old (cdr lat)))))))

(check-equal? (multi-insert-left 'cup 'and '(coffee and cereal and bread))
              '(coffee cup and cereal cup and bread))

(define multi-substitute
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new
                                 (multi-substitute new old (cdr lat))))
      (else (cons (car lat)
                  (multi-substitute new old (cdr lat)))))))

(check-equal? (multi-substitute 'or 'and '(cup and plate and fork and spoon))
              '(cup or plate or fork or spoon))