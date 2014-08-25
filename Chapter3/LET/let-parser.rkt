(module let-parser racket
  (require "let-syntax.rkt")
  (require "let-expval.rkt")
  
  (require parser-tools/lex
           (prefix-in : parser-tools/lex-sre)
           parser-tools/yacc)
  (require racket/match)
  
  (provide scan&parse)
  
  (define-tokens primitives (ID INT))
  (define-empty-tokens operators (- =))
  (define-empty-tokens delimiters (COMMA LPAR RPAR EOF))
  (define-empty-tokens keywords (ZERO? IF THEN ELSE LET IN))
  
  (define let-lexer
    (lexer-src-pos
     ["-" (token--)]
     ["=" (token-=)]
     ["(" (token-LPAR)]
     [")" (token-RPAR)]
     ["," (token-COMMA)]
     ["zero?" (token-ZERO?)]
     ["if" (token-IF)]
     ["then" (token-THEN)]
     ["else" (token-ELSE)]
     ["let" (token-LET)]
     ["in" (token-IN)]
     [(:+ numeric) (token-INT (string->number lexeme))]
     [(:+ alphabetic) (token-ID (string->symbol lexeme))]
     [whitespace (return-without-pos (let-lexer input-port))]
     [(eof) (token-EOF)]))
  
  #|
  
  (define (get-tokens lexer port [close #t])
    (let ([tokens
           (let loop ([pt (lexer port)])
             (let* ([t (position-token-token pt)]
                    [tn (token-name t)])
               (cond
                 [(eq? tn 'EOF) '(EOF)]
                 [else
                  (cons t (loop (lexer port)))])))])
      (begin
        [if close (close-input-port port) (void)]
        tokens)))
  
  (define (let-lex-port port [close #t])
    (get-tokens let-lexer port close))
  
  (define (let-lex-string str)
    (let-lex-port (open-input-string str)))

  (let-lex-string "-(55, -(x,11))")
|#
  
  (define let-parser
    (parser
     [src-pos]
     [start prog]
     [end EOF]
     [error (void)]
     [tokens primitives operators delimiters keywords]
     [grammar
      (prog [(expr) (a-program $1)])
      (expr [(INT) (const-exp $1)]
            [(- LPAR expr COMMA expr RPAR) (diff-exp $3 $5)]
            [(ZERO? LPAR expr RPAR) (zero?-exp $3)]
            [(IF expr THEN expr ELSE expr) (if-exp $2 $4 $6)]
            [(ID) (var-exp $1)]
            [(LET ID = expr IN expr) (let-exp $2 $4 $6)])]))
  
#|
  
  (define (get-ast parser lexer port [close #t])
    (let ([ast (parser (lambda () (lexer port)))])
      (begin
        [if close (close-input-port port) (void)]
        ast)))
  
  (define (let-parse-port port [close #t])
    (get-ast let-parser let-lexer port close))
  
  (define (let-parse-string str)
    (let-parse-port (open-input-string str)))
  
  (let-parse-string "-(55, -(x,11))")
  
  (let-parse-string "let z = 5
                     in let x = 3
                        in let y = -(x, 1)
                           in let x = 4
                              in -(z, -(x,y))")

|#
  
  (define (scan&parse str)
    (let* ([port (open-input-string str)]
           [ast (let-parser (lambda () (let-lexer port)))])
      (begin
        (close-input-port port)
        ast)))
  )