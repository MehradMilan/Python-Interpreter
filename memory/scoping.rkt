#lang racket
(require (lib "eopl.ss" "eopl"))
(require "../datatypes.rkt")
(require "environment.rkt")


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

;;; (define (apply-scope-index scope-index var)
;;;   (
;;;    let ([my-scope (get-scope-by-index scope-index)])
;;;     (
;;;      if (is-global? var scope-index) 
;;;      (apply-scope-index ROOT var)
;;;      (
;;;       let ([res (apply-env (scope->env my-scope) var)])
;;;        (cond
;;;          [(not (equal? res (new-env-not-found))) res]
;;;          [(>= (scope->parent-index my-scope) 0) (apply-scope-index (scope->parent-index my-scope) var)]
;;;          [else (report-not-found my-scope var)]
;;;          )
;;;       )
;;;      )
;;;     )
;;;   )


(define (extend-scope scope-index var value)
    (let ([current-scope (get-scope scope-index)])
        (let ([current-env (scope->env current-scope)])
            (let  ([new-env (extend-env var (a-promise value current-env) current-env)])
            
                (set-scope scope-index 
                    (the-scope new-env 
                        (scope->upper current-scope)
                        (scope->globals current-scope)
                    )
                )
            )
        )
    )
)

(define (is-global? var scope-index)
  (member var (scope->globals (get-scope scope-index)))
)

(provide (all-defined-out))