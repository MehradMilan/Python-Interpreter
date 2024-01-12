; Group members:
;   1.  Ali Rahmizad: 99100393
;   2.  Ali Safarafard: 99105583
;   3.  Mehrad Milanloo: 99105775

#lang racket

(require (lib "eopl.ss" "eopl"))
(require "../memory/scoping.rkt")
(require "../datatypes.rkt")


;;; (define value-of-program
;;;   (lambda (_program)
;;;     (cases program _program)
;;;       (prog (_statements) (value-of-statements _statements))))

;;; (define value-of-statements
;;;   (lambda (_statements)
;;;     (cases statements _statements)
;;;       (stmt (_statement) (value-of-statement _statement))
;;;       (stmts (_statements _statement) (begin (value-of-statements _statements) (value-of-statement _statement)))))

;;; (define value-of-statement
;;;   (lambda (_statement)
;;;     (cases statement _statement)
;;;       (stmt-compound-stmt (_compound-stmt) (value-of-compound-stmt _compound-stmt))
;;;       (stmt-simple-stmt (_simple-stmt) (value-of-simple-stmt _simple-stmt))))

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



(define run-assign 
    (lambda (var expr scope-index) 
    (begin
        (let ([index (if
                        (is-global? var scope-index)
                             0
                             scope-index)])
                (extend-scope index var expr)
        
        )
        (display "\n\n\n env \n\n\n")
        (display (get-scope scope-index))
    )
    )
)


(define run-print 
    (lambda (exprs)
        (cases expression* exprs
            (empty-expr () (void))
            (expressions (expr rest-exprs) 
                (begin
                    (run-print rest-exprs)
                    (display expr)
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
        (print_stmt (expressions) (run-print expressions))
        (else (display "error\n"))
        
        )
    )
)

(provide (all-defined-out))