; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (map box vals) env)))

(define make-init-env
  (lambda ()
    (extend-env            
     *prim-proc-names*  
     (map prim-proc      
          *prim-proc-names*)
     (empty-env))))

(define reset-global-env
  (lambda () 
    (set! global-env (make-init-env))))

(define update-global-env
  (lambda (sym val)
    (cases environment global-env
      [extended-env-record (syms vals env)
        (if (member sym syms)
          (set-ref!
            (apply-global-env sym)
            val)
          (set! 
            global-env
            (extended-env-record
              (cons sym syms)
              (cons (box val) vals)
              (empty-env))))]
      [empty-env-record ()     
        (eopl:error 'global-env "This should never happen")])))
    

(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
	    [(eq? sym (car los)) pos]
	    [else (loop (cdr los) (add1 pos))]))))
	    
(define apply-env-ref
  (lambda (env sym) 
    (cases environment env 
      [empty-env-record ()      
        (apply-global-env sym) ]
      [extended-env-record (syms vals env)
	      (let ((pos (list-find-position sym syms)))
      	    (if (number? pos)
	            (list-ref vals pos)
	            (apply-env-ref env sym)))])))

(define apply-env
  (lambda (env sym)
    (unbox (apply-env-ref env sym))))

(define set-ref!
  (lambda (ref val)
    (set-box! ref val)))

(define apply-global-env
  (lambda (sym) 
    (cases environment global-env 
      [extended-env-record (syms vals env)
	      (let ([pos (list-find-position sym syms)])
      	  (if (number? pos)
	          (list-ref vals pos)
	          (eopl:error 'global-env
			           "Symbol ~s is not bound in global env"
			            sym)))]
      [empty-env-record ()     
        (eopl:error 'global-env "This should never happen")])))


