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

; Exercise 1.33 [**]
(check-equal? (mark-leaves-with-red-depth
               (interior-node 'red
                              (interior-node 'bar
                                             (leaf 26)
                                             (leaf 12))
                              (interior-node 'red
                                             (leaf 11)
                                             (interior-node 'quux
                                                            (leaf 117)
                                                            (leaf 14)))))
              '(red
                (bar 1 1)
                (red 2 (quux 2 2))))

; Exercise 1.34 [***]
(check-equal? (path 17 '(14 (7 () (12 () ()))
                            (26 (20 (17 () ())
                                    ())
                                (31 () ()))))
              '(right left left))

; Exercise 1.35 [***]
(check-equal? (number-leaves
               (interior-node
                'foo
                (interior-node
                 'bar
                 (leaf 26)
                 (leaf 12))
                (interior-node
                 'baz
                 (leaf 11)
                 (interior-node
                  'quux
                  (leaf 117)
                  (leaf 14)))))
              '(foo
                (bar 0 1)
                (baz 2 (quux 3 4))))

; Exercise 1.36 [***]
(define number-elements-from
  (lambda (lst n)
    (if (null? lst)
        '()
        (cons
         (list n (car lst))
         (number-elements-from (cdr lst) (+ n 1))))))

(define number-elements
  (lambda (lst)
    (number-elements-from lst 0)))

(define number-elements.
  (lambda (lst)
    (if (null? lst)
        '()
        (g (list 0 (car lst)) (number-elements. (cdr lst))))))

(check-equal? (number-elements. '(a b c d))
              (number-elements '(a b c d)))