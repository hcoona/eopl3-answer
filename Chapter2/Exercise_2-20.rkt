#lang eopl

(provide number->bintree*
         insert-to-left
         insert-to-right
         move-to-left-son
         move-to-right-son
         move-up
         current-element
         at-root?
         at-leaf?)

; Bintree  ::= () | (Int Bintree Bintree)
; BintreeP ::= ('LEFT-SON-REMOVED  Int Bintree)
;            | ('RIGHT-SON-REMOVED Int Bintree)
; Bintree* ::= ('LEAF Listof(BintreeP))
;            | (Int   Listof(BintreeP) Bintree Bintree)

(define (report-extract-content-from-leaf-node-error proc-name value-name)
  (eopl:error proc-name "Leaf node contains no ~s." value-name))

(define (bintree-leaf) '())
(define (bintree-leaf? t) (null? t))
(define (bintree-interior-node n l r) (list n l r))
(define (bintree->number t)
  (cond [(bintree-leaf? t) (report-extract-content-from-leaf-node-error 'bintree->number 'value)]
        [else (car t)]))
(define (bintree->left-son t)
  (cond [(bintree-leaf? t) (report-extract-content-from-leaf-node-error 'bintree->left-son 'left-son)]
        [else (cadr t)]))
(define (bintree->right-son t)
  (cond [(bintree-leaf? t) (report-extract-content-from-leaf-node-error 'bintree->right-son 'right-son)]
        [else (caddr t)]))

(define (bintree*-leaf p-list) `(LEAF ,p-list))
(define (bintree*-leaf? t) (eq? 'LEAF (car t)))
(define (bintree*-interior-node n p-list l r) (list n p-list l r))
(define (bintree*->number t)
  (cond [(bintree*-leaf? t) (report-extract-content-from-leaf-node-error 'bintree*->number 'value)]
        [else (car t)]))
(define (bintree*->parent-list t) (cadr t))
(define (bintree*->left-son t)
  (cond [(bintree*-leaf? t) (report-extract-content-from-leaf-node-error 'bintree*->left-son 'left-son)]
        [else (caddr t)]))
(define (bintree*->right-son t)
  (cond [(bintree*-leaf? t) (report-extract-content-from-leaf-node-error 'bintree*->right-son 'right-son)]
        [else (cadddr t)]))

(define (number->bintree* n) (bintree*-interior-node n '() '() '()))
(define (bintree->bintree* t parent-list)
  (cond [(bintree-leaf? t) (bintree*-leaf parent-list)]
        [else
         (bintree*-interior-node (bintree->number t)
                                 parent-list
                                 (bintree->left-son t)
                                 (bintree->right-son t))]))

(define (bintree*->bintreeP t cut-son)
  (cond [(eq? 'LEFT cut-son)
         `(LEFT-SON-REMOVED ,(bintree*->number t) ,(bintree*->right-son t))]
        [(eq? 'RIGHT cut-son)
         `(RIGHT-SON-REMOVED ,(bintree*->number t) ,(bintree*->left-son t))]
        [else
         (eopl:error 'bintree*->bintreeP "cut-son can be either 'LEFT or 'RIGHT: ~s" cut-son)]))

(define (bintreeP->number t) (cadr t))
(define (bintreeP->son t) (caddr t))
(define (bintreeP->bintree* t p-list son)
  (cond [(eq? 'LEFT-SON-REMOVED (car t))
         (bintree*-interior-node (bintreeP->number t)
                                 p-list
                                 son
                                 (bintreeP->son t))]
        [(eq? 'RIGHT-SON-REMOVED (car t))
         (bintree*-interior-node (bintreeP->number t)
                                 p-list
                                 (bintreeP->son t)
                                 son)]
        [else
         (eopl:error 'bintreeP->bintree* "Invalid BinTreeP : ~s" t)]))

(define (at-root? t)
  (null? (bintree*->parent-list t)))

(define (at-leaf? t) (bintree*-leaf? t))

(define (current-element t) (bintree*->number t))

(define (move-to-left-son t)
  (cond [(at-leaf? t) (eopl:error 'move-to-left-son "Leaf node has no left son")]
        [else (let ([p-list   (bintree*->parent-list t)]
                    [left-son (bintree*->left-son t)])
                (bintree->bintree* left-son (cons (bintree*->bintreeP t 'LEFT) p-list)))]))

(define (move-to-right-son t)
  (cond [(at-leaf? t) (eopl:error 'move-to-right-son "Leaf node has no right son.")]
        [else (let ([p-list    (bintree*->parent-list t)]
                    [right-son (bintree*->right-son t)])
                (bintree->bintree* right-son (cons (bintree*->bintreeP t 'RIGHT) p-list)))]))

(define (move-up t)
  (cond [(at-root? t) (eopl:error 'move-up "Root node cannot move up.")]
        [else (let ([p-list (bintree*->parent-list t)])
                (bintreeP->bintree* (car p-list)
                                    (cdr p-list)
                                    (cond [(bintree*-leaf? t) (bintree-leaf)]
                                          [else (bintree-interior-node (bintree*->number t)
                                                                       (bintree*->left-son t)
                                                                       (bintree*->right-son t))])))]))

(define (insert-to-left n t)
  (bintree*-interior-node (bintree*->number t)
                          (bintree*->parent-list t)
                          (let ([left-son (bintree*->left-son t)])
                            (bintree-interior-node n
                                                   left-son
                                                   (bintree-leaf)))
                          (bintree*->right-son t)))
(define (insert-to-right n t)
  (bintree*-interior-node (bintree*->number t)
                          (bintree*->parent-list t)
                          (bintree*->left-son t)
                          (let ([right-son (bintree*->right-son t)])
                            (bintree-interior-node n
                                                   (bintree-leaf)
                                                   right-son))))

(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree* 13))))