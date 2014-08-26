(module let-parser racket
  (require "let-syntax.rkt")
  (require "let-expval.rkt")
  
  (require parser-tools/lex
           (prefix-in : parser-tools/lex-sre)
           parser-tools/yacc)
  
  (provide scan&parse)
  
  (define-tokens primitives (ID INT))
  (define-empty-tokens delimiters (COMMA LPAR RPAR EOF))
  (define-empty-tokens keywords (IF THEN ELSE LET ASSIGN IN))
  (define-empty-tokens functions (+ - * / ZERO? MINUS EQUAL? GREATER? LESS? CONS CAR CDR NULL? EMPTYLIST LIST))
  
  (define let-lexer
    (lexer-src-pos
     [(:or "+" "-" "*" "/") (string->symbol lexeme)]
     ["=" (token-ASSIGN)]
     ["(" (token-LPAR)]
     [")" (token-RPAR)]
     ["," (token-COMMA)]
     ["zero?" (token-ZERO?)]
     ["minus" (token-MINUS)]
     ["equal?" (token-EQUAL?)]
     ["greater?" (token-GREATER?)]
     ["less?" (token-LESS?)]
     ["cons" (token-CONS)]
     ["car" (token-CAR)]
     ["cdr" (token-CDR)]
     ["null?" (token-NULL?)]
     ["emptylist" (token-EMPTYLIST)]
     ["list" (token-LIST)]
     ["if" (token-IF)]
     ["then" (token-THEN)]
     ["else" (token-ELSE)]
     ["let" (token-LET)]
     ["in" (token-IN)]
     [(:+ numeric) (token-INT (string->number lexeme))]
     [(:+ alphabetic) (token-ID (string->symbol lexeme))]
     [whitespace (return-without-pos (let-lexer input-port))]
     [(eof) (token-EOF)]))
  
  (define let-parser
    (parser
     [src-pos]
     [start prog]
     [end EOF]
     [error (void)]
     [tokens primitives delimiters keywords functions]
     [grammar
      (prog [(expr) (a-program $1)])
      (expr [(INT) (const-exp $1)]
            [(func-name) (apply-exp $1 '())]
            [(func-name parameter-list) (apply-exp $1 $2)]
            [(IF expr THEN expr ELSE expr) (if-exp $2 $4 $6)]
            [(ID) (var-exp $1)]
            [(LET ID ASSIGN expr IN expr) (let-exp $2 $4 $6)])
      (func-name [(+) '+]
                 [(-) '-]
                 [(*) '*]
                 [(/) '/]
                 [(ZERO?) 'zero?]
                 [(MINUS) 'minus]
                 [(EQUAL?) 'equal?]
                 [(GREATER?) 'greater?]
                 [(LESS?) 'less?]
                 [(CONS) 'cons]
                 [(CAR) 'car]
                 [(CDR) 'cdr]
                 [(NULL?) 'null?]
                 [(EMPTYLIST) 'emptylist]
                 [(LIST) 'list])
      (parameter-list [(LPAR RPAR) '()]
                      [(LPAR expr-comma-list RPAR) $2])
      (expr-comma-list [(expr) (list $1)]
                       [(expr COMMA expr-comma-list) (cons $1 $3)])]))
  
  (define (scan&parse str)
    (let* ([port (open-input-string str)]
           [ast (let-parser (lambda () (let-lexer port)))])
      (begin
        (close-input-port port)
        ast)))
  )