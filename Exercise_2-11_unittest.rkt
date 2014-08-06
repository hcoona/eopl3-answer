#lang racket
(require rackunit
         "Exercise_2-11.rkt")

(define env
  (extend-env* '(a b c)
               '(11 22 33)
               (extend-env* '(x z)
                            '(66 77)
                            (extend-env* '(x y)
                                         '(88 99)
                                         (extend-env 's
                                                     5
                                                     (empty-env))))))

(check-equal? (apply-env env 'a) 11)
(check-equal? (apply-env env 'b) 22)
(check-equal? (apply-env env 'c) 33)
(check-equal? (apply-env env 's) 5)
(check-equal? (apply-env env 'x) 66)
(check-equal? (apply-env env 'y) 99)
(check-equal? (apply-env env 'z) 77)