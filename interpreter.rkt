; Group members:
;   1.  Ali Rahmizad: 99100393
;   2.  Ali Safarafard: 99105583
;   3.  Mehrad Milanloo: 99105775

#lang racket

(require (lib "eopl.ss" "eopl"))

(define value-of-program
  (lambda (_program)
    (cases program _program)
      (prog (_statements) (value-of-statements _statements))))

(define value-of-statements
  (lambda (_statements)
    (cases statements _statements)
      (stmt (_statement) (value-of-statement _statement))
      (stmts (_statements _statement) (begin (value-of-statements _statements) (value-of-statement _statement)))))

(define value-of-statement
  (lambda (_statement)
    (cases statement _statement)
      (stmt-compound-stmt (_compound-stmt) (value-of-compound-stmt _compound-stmt))
      (stmt-simple-stmt (_simple-stmt) (value-of-simple-stmt _simple-stmt))))
