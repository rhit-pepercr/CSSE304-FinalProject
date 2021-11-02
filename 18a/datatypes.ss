
;; Parsed expression datatype.  You will probably want to replace this
;; with your expression datatype from A11b.

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lambda-exp
   (ids (list-of symbol?))
   (bodies (list-of expression?))]
  [lambda-n-exp
    (id symbol?)
    (bodies (list-of expression?))]
  [lambda-imp-exp
    (ids (list-of symbol?))
    (opt-id symbol?)
    (bodies (list-of expression?))]
  [let-binding-exp
    (id symbol?)
    (binding expression?)]
  [let-exp
    (bindings (list-of expression?))
    (bodies (list-of expression?))]
  [let*-exp 
    (bindings (list-of expression?))
    (bodies (list-of expression?))]
  [letrec-exp
    (bindings (list-of expression?))
    (bodies (list-of expression?))]
  [named-let-exp
    (name symbol?)
    (bindings (list-of expression?))
    (bodies (list-of expression?))]
  [app-exp
   (operator expression?)
   (operands (list-of expression?))]
  [lit-exp
    (id (lambda (x) 
          (ormap (lambda (pred) 
            (pred x)) 
            (list number? string? symbol? list? boolean? vector?))))]
  [if-exp
    (condition expression?)
    (then expression?)
    (else expression?)]
  [set!-exp
    (id symbol?)
    (expression expression?)]
  [begin-exp
    (expressions (list-of expression?))]
  [cond-exp
    (clauses (list-of expression?))]
  [cond-clause 
    (test expression?)
    (bodies (list-of expression?))]
  [and-exp
    (clauses (list-of expression?))]
  [or-exp 
    (clauses (list-of expression?))]
  [while-exp
    (test expression?)
    (bodies (list-of expression?))]
  [define-exp
    (id symbol?)
    (expression expression?)])

 	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure 
    (ids (list-of symbol?))
    (bodies (list-of expression?))
    (env environment?)]
  [n-closure
    (id symbol?)
    (bodies (list-of expression?))
    (env environment?)]
  [imp-closure
    (ids (list-of symbol?))
    (opt-id symbol?)
    (bodies (list-of expression?))
    (env environment?)]
  [k-proc 
    (k continuation?)])

	 
	 
	 
