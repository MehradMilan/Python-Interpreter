#lang racket

(require "passes/parser.rkt")
(require "passes/lexer.rkt")
(require "passes/interpreter.rkt")

(define (parse-scan prog-string)
  (python-parser (lex-this prog-string))
  )

(define (evaluate file-name)
  (run (parse-scan (string-join (file->lines file-name))))
  )

(evaluate "tests/test.py")

(provide (all-defined-out))
