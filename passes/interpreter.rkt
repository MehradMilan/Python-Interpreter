; Group members:
;   1.  Ali Rahmizad: 99100393
;   2.  Ali Safarafard: 99105583
;   3.  Mehrad Milanloo: 99105775

#lang racket

(require (lib "eopl.ss" "eopl"))
(require "../memory/scoping.rkt")


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



(define run-single-command 
    (lambda (command scope-index)
        (display command)
    )
)

(provide (all-defined-out))