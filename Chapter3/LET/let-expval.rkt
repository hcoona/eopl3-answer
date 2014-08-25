(module let-expval eopl
  (provide expval
           expval?
           num-val
           bool-val
           expval->num
           expval->bool)
  
  (define bool? boolean?)
  
  ;; Page 70
  (define-datatype expval expval?
    [num-val (num number?)]
    [bool-val (bool bool?)])
  
  (define (expval->num val)
    (cases expval val
      [num-val (num) num]
      [else (report-expval-extractor-error 'num val)]))
  
  (define (expval->bool val)
    (cases expval val
      [bool-val (bool) bool]
      [else (report-expval-extractor-error 'bool val)]))
  
  (define report-expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))
  )