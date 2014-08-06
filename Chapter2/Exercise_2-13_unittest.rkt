#lang racket
(require rackunit)
(require "Exercise_2-13.rkt")

(define env
  (extend-env 'a 1
              (extend-env 'b 2
                          (extend-env 'c 3
                                      (extend-env 'b 2
                                                  (empty-env))))))

(check-true   (empty-env? (empty-env)))
(check-false  (empty-env? env))
(check-equal? (apply-env env 'a) 1)
(check-equal? (apply-env env 'b) 2)
(check-equal? (apply-env env 'c) 3)