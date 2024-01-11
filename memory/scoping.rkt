#lang racket
(require (lib "eopl.ss" "eopl"))
(require "../datatypes.rkt")

(define scopes '())

(define scope-index? (lambda (n) (< n (length scopes))))



(define-datatype scope scope?
  (the-scope (env environment?) (upper-scope-index scope-index?) (globals list?))
  )

(define (scope->env s)
  (cases scope s
    (the-scope (env upper-scope-index globals) env)
    ))

(define (scope->upper s)
  (cases scope s
    (the-scope (env upper-scope-index globals) upper-scope-index)
    ))

(define (scope->globals s)
  (cases scope s
    (the-scope (env upper-scope-index globals) globals)
    ))


(define correct-index? (lambda (n) (< n (length scopes))))

(define (get-scope index)
  (list-ref scopes index)
)

(define (set-scope index s)
  (set! scopes
        (list-set scopes index s)
        ))

(define (add-scope s)
  (begin
    (set! scopes (append scopes (list s)))
    (- (length scopes) 1)
    ))

(define (renew-scope) (set! scopes '()))

(define (init-scope) (the-scope (empty-environment) -1 '()))

(provide (all-defined-out))