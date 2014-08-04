#lang racket
(require rackunit
         "chap1.rkt")

; Exercise 1.15 [*]
(check-equal? (duple 2 3)
              '(3 3))

(check-equal? (duple 4 '(ha ha))
              '((ha ha) (ha ha) (ha ha) (ha ha)))

(check-equal? (duple 0 '(blah))
              '())

; Exercise 1.16 [*]
(check-equal? (invert '((a 1) (a 2) (1 b) (2 b)))
              '((1 a) (2 a) (b 1) (b 2)))

; Exercise 1.17 [*]
(check-equal? (down '(1 2 3))
              '((1) (2) (3)))

(check-equal? (down '((a) (fine) (idea)))
              '(((a)) ((fine)) ((idea))))

(check-equal? (down '(a (more (complicated)) object))
              '((a) ((more (complicated))) (object)))

; Exercise 1.18 [*]
; Skip swapper

; Exercise 1.19 [**]
(check-equal? (list-set '(a b c d) 2 '(1 2))
              '(a b (1 2) d))

(check-equal? (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)
              '(1 5 10))

; Skip Exercise 1.20 ~ 1.31

; Exercise 1.32 [*]
(check-equal? (double-tree '(bar
                             (bar 1 (foo 1 2))
                             (biz 4 5)))
              '(bar
                (bar 2 (foo 2 4))
                (biz 8 10)))