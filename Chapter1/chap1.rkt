#lang eopl

(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))

(define (foldl f init lst)
  (cond
    [(null? lst) init]
    [else (foldl f (f (car lst) init) (cdr lst))]))

(define (foldr f init lst)
  (cond
    [(null? lst) init]
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
      [(leaf? t) (leaf (double (contents-of t)))]
      [else (interior-node
             (contents-of t)
             (double-tree (lson t))
             (double-tree (rson t)))])))

(define (mark-leaves-with-red-depth t)
  (let loop ([counter 0] [t t])
    (cond
      [(leaf? t) (leaf counter)]
      [else (let* ([s (contents-of t)]
                   [c (if (eq? s 'red)
                          (add1 counter)
                          counter)])
              (interior-node
               s
               (loop c (lson t))
               (loop c (rson t))))])))

(define (path n bst)
  (let ([s (contents-of bst)])
    (cond
      [(= n s) '()]
      [(< n s) (cons 'left (path n (lson bst)))]
      [(> n s) (cons 'right (path n (rson bst)))])))

(define (number-leaves bt)
  (let ([c 0])
    (let loop ([bt bt])
      (cond
        [(leaf? bt) (let ([c0 c])
                      (begin
                        (set! c (add1 c))
                        (leaf c0)))]
        [else (interior-node
               (contents-of bt)
               (loop (lson bt))
               (loop (rson bt)))]))))

#|
; CPS style loop without changing any values
(define (number-leaves bt)
  (let loop ([bt bt]
             [c 0]
             [cont (lambda (x c) x)])
    (cond
      [(leaf? bt) (cont (leaf c) (add1 c))]
      [else
       (loop (lson bt)
             c
             (lambda (l c)
               (loop (rson bt)
                     c
                     (lambda (r c)
                       (cont (interior-node (contents-of bt) l r)
                             c)))))])))
|#

(define (g hd tl)
  (cons
   hd
   (if (null? tl)
      '()
      (let add-from-n-to-elements ([n (add1 (car hd))]
                                   [lst tl])
        (if (null? lst)
            '()
            (cons (cons n (cdar lst))
                  (add-from-n-to-elements (add1 n) (cdr lst))))))))

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
(provide double-tree
         mark-leaves-with-red-depth
         path
         number-leaves
         g)