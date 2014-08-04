#lang eopl

(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))

(define (foldl f init lst)
  (cond
    [(eq? lst '()) init]
    [else (foldl f (f (car lst) init) (cdr lst))]))

(define (foldr f init lst)
  (cond
    [(eq? lst '()) init]
    [else (f (car lst) (foldr f init (cdr lst)))]))

(define (duple n x)
  (cond
    [(= n 0) '()]
    [else (cons x (duple (- n 1) x))]))

(define (invert lst)
  (map reverse lst))

(define (down lst)
  (map (lambda (x) (list x)) lst))

(define swapper '()) ; Skip

(define (list-set lst n x)
  (let loop ([lst lst] [n n])
    (cond
      [(= n 0) (cons x (cdr lst))]
      [else (cons (car lst)
                  (loop (cdr lst) (sub1 n)))])))

#|
(define (list-set lst n x)
  (if (= n 0)
      (cons x (cdr lst))
      (cons (car lst)
            (list-set (cdr lst) (sub1 n) x))))
|#

; Skip Exercise 1.20 ~ 1.30

; Exercise 1.31
(define (leaf n) n)
(define (interior-node s l r)
  (list s l r))
(define leaf? number?)
(define lson cadr)
(define rson caddr)
(define (contents-of t)
  (cond
    [(leaf? t) t]
    [else (car t)]))

; Exercise 1.32
(define (double-tree t)
  (let ([double (lambda (n) (* 2 n))])
    (cond
      [(leaf? t) (double (contents-of t))]
      [else (interior-node
             (contents-of t)
             (double-tree (lson t))
             (double-tree (rson t)))])))

(provide duple
         invert
         down
         swapper
         list-set)
(provide leaf
         interior-node
         leaf?
         lson
         rson
         contents-of)
(provide double-tree)