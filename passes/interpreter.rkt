; Group members:
;   1.  Ali Rahmizad: 99100393
;   2.  Ali Safarafard: 99105583
;   3.  Mehrad Milanloo: 99105775

#lang racket

(require (lib "eopl.ss" "eopl"))
(require "../memory/scoping.rkt")
(require "../datatypes.rkt")



(define run 
    (lambda (abstract-syntax-tree)
    (begin
        (renew-scope)
        (interp-ast abstract-syntax-tree (add-scope (init-scope)))
    )
    )
)

(define interp-ast 
    (lambda (as-subtree scope-index)
        (if (null? as-subtree) 
            (void)
            (let (
                [root (car as-subtree)]
                [new-subtree (cdr as-subtree)]
                )
                (let ([root-result (run-single-command root scope-index)]) 
                    (if (or (eq? root-result `break) (eq? root-result `continue))
                        (void)
                        (interp-ast new-subtree scope-index)
                    )
                )
            )
        )
    )
)
(define eval-binary-op 
    (lambda (op left right scope-index)
        (let ([left-value (eval-expr left scope-index)]
              [op-name (object-name op)])
            (cond
                [(eq? op-name '*)
                    (if (zero? left-value)
                        left-value
                        (op left-value (eval-expr right scope-index))
                    )
                ]
                [(eq? op-name 'expt)
                    (if (or (eq? left-value 1) (eq? left-value 0)) 
                        left-value
                        (op left-value (eval-expr right scope-index))
                    )
                ]
                [(eq? op-name 'and-op)
                    (if (eq? left-value #f)
                        #f
                        (op left-value (eval-expr right scope-index))
                    )
                ]
                [(eq? op-name 'or-op)
                    (if (eq? left-value #t)
                        #t
                        (op left-value (eval-expr right scope-index))
                    )
                ]
                [else
                    (op left-value (eval-expr right scope-index))    
                ]
            )
        )
    )
)

(define eval-binary-op-promise 
    (lambda (op left right scope-index -scopes)
        (let ([left-value (eval-expr-promise left scope-index -scopes)]
              [op-name (object-name op)])
            (cond
                [(eq? op-name '*)
                    (if (zero? left-value)
                        left-value
                        (op left-value (eval-expr-promise right scope-index -scopes))
                    )
                ]
                [(eq? op-name 'expt)
                    (if (or (eq? left-value 1) (eq? left-value 0)) 
                        left-value
                        (op left-value (eval-expr-promise right scope-index -scopes))
                    )
                ]
                [(eq? op-name 'and-op)
                    (if (eq? left-value #f)
                        #f
                        (op left-value (eval-expr-promise right scope-index -scopes))
                    )
                ]
                [(eq? op-name 'or-op)
                    (if (eq? left-value #t)
                        #t
                        (op left-value (eval-expr-promise right scope-index -scopes))
                    )
                ]
                [else
                    (op left-value (eval-expr-promise right scope-index -scopes))    
                ]
            )
        )
    )
)

(define run-ref
    (lambda (var scope-index) 
        ( let ([r (apply-scope scope-index var)])
        (cases promise r
            (a-promise (expr scope-index -scopes)
                (eval-expr-promise expr scope-index -scopes)
            )
            (else r)
        
        )
        )
    )
)

(define run-ref-promise
    (lambda (var scope-index -scopes) 
    ( let ([r (apply-scope-on-given-scopes scope-index -scopes var)])
        (cases promise  r
            (a-promise (expr scope-index --scopes)
                (eval-expr-promise expr scope-index --scopes)
            )
            (else r)
        )
    )
    )
)


(define (eval-expr-promise expr scope-index -scopes)
    (cases expression expr
        (binary_op (op left right) (eval-binary-op-promise op left right scope-index -scopes))
        (unary_op (op operand) (op (eval-expr-promise operand scope-index -scopes)))
        ;;; (function_call (func expression?) (params expression*?))
        ;;; (list_ref (ref expression?) (index expression?))
        (ref (var) (run-ref-promise var scope-index -scopes))
        (atomic_bool_exp (bool) bool)
        (atomic_num_exp (num) num)
        (atomic_null_exp () (void))
        ;;; (atomic_list_exp (l expression*?))
        (else (display "else\n"))
    )
)


(define (eval-expr expr scope-index)
    (cases expression expr
        (binary_op (op left right) (eval-binary-op op left right scope-index))
        (unary_op (op operand) (op (eval-expr operand scope-index)))
        ;;; (function_call (func expression?) (params expression*?))
        ;;; (list_ref (ref expression?) (index expression?))
        (ref (var) (run-ref var scope-index))
        (atomic_bool_exp (bool) bool)
        (atomic_num_exp (num) num)
        (atomic_null_exp () (void))
        ;;; (atomic_list_exp (l expression*?))
        (else (display "else\n"))
    )
)

(define run-assign 
    (lambda (var expr scope-index) 
    (begin
        (let ([index (if
                        (is-global? var scope-index)
                             0
                             scope-index)])
                (extend-scope index var expr)
        
        )
        ;;; (display "\n\n\n env \n\n\n")
        ;;; (display (get-scope scope-index))
    )
    )
)


(define run-print 
    (lambda (exprs scope-index)
        (cases expression* exprs
            (empty-expr () (void))
            (expressions (expr rest-exprs) 
                (begin
                    (run-print rest-exprs scope-index)
                    (display (eval-expr expr scope-index))
                    (display "\n")
                )
            )
        )
    )
)
(define run-single-command 
    (lambda (command scope-index)
        (cases statement command
        (assign (var expr) (run-assign var expr scope-index))
        (global (var) (display "global command\n"))
        (return (expr) (display "return command\n"))
        (return_void () (display "return void command\n"))
        (pass () (display "pass command\n"))
        (break () (display "break command\n"))
        (continue () (display "continue command\n"))
        (func (name params statements) (display "func command\n"))
        (if_stmt (cond_exp if_sts else_sts) (display "if command\n"))
        (for_stmt (iter list_exp sts) (display "for command\n"))
        (print_stmt (expressions) (run-print expressions scope-index))
        (else (display "error\n"))
        
        )
    )
)

(provide (all-defined-out))