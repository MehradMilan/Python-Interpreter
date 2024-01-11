#lang racket

(define scopes '())

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

(provide (all-defined-out))