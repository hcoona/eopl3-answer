(module let-expval eopl
  (provide expval
           expval?
           num-val
           bool-val
           list-val
           expval->num
           expval->bool
           expval->list
           expval->any)
  
  (define bool? boolean?)
  
  ;; Page 70
  (define-datatype expval expval?
    [num-val (num number?)]
    [bool-val (bool bool?)]
    [list-val (list list?)])
  
  (define (expval->num val)
    (cases expval val
      [num-val (num) num]
      [else (report-expval-extractor-error 'num val)]))
  
  (define (expval->bool val)
    (cases expval val
      [bool-val (bool) bool]
      [else (report-expval-extractor-error 'bool val)]))
  
  (define (expval->list val)
    (cases expval val
      [list-val (list) list]
      [else (report-expval-extractor-error 'list val)]))
  
  (define (expval->any val)
    (cases expval val
      [num-val (num) num]
      [bool-val (bool) bool]
      [list-val (list) list]))      
  
  (define report-expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))
  )