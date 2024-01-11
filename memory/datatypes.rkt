#lang racket
(require (lib "eopl.ss" "eopl"))

(define scope-index? (lambda (n) (< n (length scope-mem))))


(define-datatype environment environment?
  (empty-environment)
  (extended-environment (var string?) (val expression?) (env environment?))
  )

(define-datatype scope scope?
  (the-scope (env environment?) (upper-scope-index scope-index?) (globals list?))
  )

(define (scope->env s)
  (cases scope s
    (the-scope (env upper-scope-index globals) env)
    ))

(define (scope->upper s)
  (cases scope s
    (new-scope (env upper-scope-index globals) upper-scope-index)
    ))

(define (scope->globals s)
  (cases scope s
    (new-scope (env upper-scope-index globals) globals)
    ))


(provide (all-defined-out))
(#%provide (all-defined))