; When you convert your interpreter to CPS for A18, this could be a good place
; to put your apply-k procedure and your continuation datatype definition.

(define-datatype continuation continuation?
    [init-k]
    [if-k 
        (then expression?)
        (else expression?)
        (env environment?)
        (k continuation?)]
    [binding-k
        (id symbol?)
        (k continuation?)]
    [app-k
        (operator expression?)
        (env environment?)
        (k continuation?)]
    [apply-proc-k
        (operands (list-of scheme-value?))
        (k continuation?)]
    [map-k 
        (proc-cps procedure?)
        (el scheme-value?)
        (k continuation?)]
    [map-proc-k
        (mapped-cdr list?)
        (k continuation?)]
    [set!-k
        (id symbol?)
        (env environment?)
        (k continuation?)])

(define apply-k
    (lambda (k v)
        (cases continuation k
            [init-k () v]
            [if-k (then else env k)
                (if v
                    (eval-exp then env k)
                    (eval-exp else env k))]
            [binding-k (id k) 
                (apply-k k (cons id v))]
            [app-k (operator env k)
                (eval-exp
                    operator
                    env
                    (apply-proc-k v k))]
            [apply-proc-k (operands k)
                (apply-proc v operands k)]
            [map-k (proc-cps el k)
                (proc-cps
                    el
                    (map-proc-k v k))]
            [map-proc-k (mapped-cdr k)
                (apply-k k (cons v mapped-cdr))]
            [set!-k (id env k)
                (apply-k k (set-ref! (apply-env-ref env id) v))])))

(define map-cps
    (lambda (proc-cps L k)
        (if (null? L)
            (apply-k k '())
            (map-cps
                proc-cps
                (cdr L)
                (map-k proc-cps (car L) k)))))