; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
	    [(eq? sym (car los)) pos]
	    [else (loop (cdr los) (add1 pos))]))))
	    
(define apply-env
  (lambda (env sym) 
    (cases environment env 
      [empty-env-record ()      
        (apply-global-env sym) ]
      [extended-env-record (syms vals env)
	      (let ((pos (list-find-position sym syms)))
      	    (if (number? pos)
	            (list-ref vals pos)
	            (apply-env env sym)))])))

(define apply-global-env
  (lambda (sym) 
    (cases environment init-env 
      [extended-env-record (syms vals env)
	      (let ([pos (list-find-position sym syms)])
      	  (if (number? pos)
	          (list-ref vals pos)
	          (eopl:error 'global-env
			           "Symbol ~s is not bound in global env"
			            sym)))]
      [empty-env-record ()     
        (eopl:error 'global-env "This should never happen")])))

