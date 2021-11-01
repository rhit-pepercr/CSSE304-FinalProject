; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (cases expression form
      [define-exp (id expression) 
        (update-global-env 
          id
          (eval-exp expression (empty-env-record)))]
      [app-exp (operator operands)
        (if (null? operands)
          (top-level-eval operator)
          (eval-exp form (empty-env-record)))]
      [lambda-exp (ids bodies)
        (if (null? ids)
          (for-each (lambda (body) (top-level-eval body)) bodies)
          (eval-exp form (empty-env-record)))]
      [else (eval-exp form (empty-env-record))])))


; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum)
        (if (pair? datum)
          (if (eqv? (car datum) 'quote)
            (cadr datum))
          datum)]

      [var-exp (id)
	      (apply-env env id)]

      [if-exp (condition then-exp else-exp)
        (if (eval-exp condition env)
          (eval-exp then-exp env)
          (eval-exp else-exp env))]

      [let-binding-exp (id binding)
        (cons id (eval-exp binding env))]

      [app-exp (operator operands)
        (let ([proc-value (eval-exp operator env)]
              [args (map (lambda (operand) (eval-exp operand env)) operands)])
          (apply-proc proc-value args))]

      [lambda-exp (ids bodies)
        (closure ids bodies env)]
      
      [lambda-n-exp (id bodies)
        (n-closure id bodies env)]

      [lambda-imp-exp (ids opt-id bodies)
        (imp-closure ids opt-id bodies env)]

      [set!-exp (id expression)
        (set-ref!
          (apply-env-ref env id)
          (eval-exp expression env))]

      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define split-for-imp
  (lambda (lst proper-len)
    (if (zero? proper-len)
      (list lst)
      (cons (car lst) (split-for-imp (cdr lst) (sub1 proper-len))))))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]

      [closure (ids bodies env)
        (let ([new-env (extend-env ids args env)])
          (for-each (lambda (body) (eval-exp body new-env)) bodies))]

      [n-closure (id bodies env)
        (let ([new-env (extend-env (list id) (list args) env)])
          (for-each (lambda (body) (eval-exp body new-env)) bodies))]

      [imp-closure (ids opt-id bodies env)
        (let* ([split-args (split-for-imp args (length ids))]
               [new-env (extend-env (append ids (list opt-id)) split-args env)])
          (for-each (lambda (body) (eval-exp body new-env)) bodies))]

			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* 
  '(+ - * / add1 sub1 cons = zero? not >= > < <= car cdr list null? eq? equal? length list->vector 
    list? not pair? vector->list number? vector? symbol? caar cadr cdar cddr caaar caadr cadar caddr 
    cdaar cdadr cddar cdddr procedure? set-car! set-cdr! assq atom? vector make-vector vector-ref 
    vector-set! display newline void map apply negative? positive? quotient append eqv? list-tail))

(define global-env         
  (make-init-env))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [(zero?) (= (1st args) 0)]
      [(>=) (>= (1st args) (2nd args))]
      [(>) (> (1st args) (2nd args))]
      [(<) (< (1st args) (2nd args))]
      [(<=) (<= (1st args) (2nd args))]
      [(car) (car (1st args))]
      [(cdr) (cdr (1st args))]
      [(list) (apply list args)]
      [(null?) (eq? (1st args) '())]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(not) (if (1st args) #f #t)]
      [(pair?) (apply pair? args)]
      [(vector->list) (vector->list (1st args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(caar) (car (car (1st args)))]
      [(cadr) (car (cdr (1st args)))]
      [(cddr) (cdr (cdr (1st args)))]
      [(cdar) (cdr (car (1st args)))]
      [(caaar) (car (car (car (1st args))))]
      [(caadr) (car (car (cdr (1st args))))]
      [(cadar) (car (cdr (car (1st args))))]
      [(caddr) (car (cdr (cdr (1st args))))]
      [(cdaar) (cdr (car (car (1st args))))]
      [(cdadr) (cdr (car (cdr (1st args))))]
      [(cddar) (cdr (cdr (car (1st args))))]
      [(cdddr) (cdr (cdr (cdr (1st args))))]
      [(procedure?) (proc-val? (1st args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(assq) (assq (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(vector) (apply vector args)]
      [(make-vector) (apply make-vector args)]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (apply display args)]
      [(newline) (newline)]
      [(void) (void)]
      [(map) (map (lambda (arg) (apply-proc (1st args) (list arg))) (2nd args))]
      [(apply) (apply-proc (1st args) (2nd args))]
      [(negative?) (negative? (1st args))]
      [(positive?) (positive? (1st args))]
      [(quotient) (quotient (1st args) (2nd args))]
      [(append) (apply append args)]
      [(eqv?) (eqv? (1st args) (2nd args))]
      [(list-tail) (list-tail (1st args) (2nd args))]

      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    (let ([input (read)])
      (if (equal? input '(exit))
        (eopl:error 'rep "Successfully Exited rep")
        ;; notice that we don't save changes to the environment...
        (let ([answer (top-level-eval (syntax-expand (parse-exp input)))])
          (if (proc-val? answer)
            (eopl:pretty-print '<interpreter-procedure>)
            (eopl:pretty-print answer))
          (newline)
          (rep))))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))










