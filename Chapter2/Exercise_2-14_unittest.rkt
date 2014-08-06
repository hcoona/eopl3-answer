#lang racket
(require rackunit)
(require "Exercise_2-14.rkt")

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
(check-true   (has-binding? env 'a))
(check-true   (has-binding? env 'b))
(check-true   (has-binding? env 'c))
(check-false  (has-binding? env 'd))
(check-false  (has-binding? env 'z))