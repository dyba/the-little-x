(module utils racket
  (provide (all-defined-out))

  (define scheme-atom?
    (lambda (x)
      (and (not (pair? x))
           (not (null? x))))))