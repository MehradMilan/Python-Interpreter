#lang racket

(require "passes/parser.rkt")
(require "passes/lexer.rkt")
(require "passes/interpreter.rkt")

(define (parse-scan prog-string)
  (python-parser (lex-this prog-string))
  )

(define (print-ast file-name) (display (parse-scan (string-join (file->lines file-name)))))
(define (evaluate file-name)
  (run (parse-scan (string-join (file->lines file-name))))
  )

;;; (parse-scan "tests/test.py")

(evaluate "tests/test.py")

(provide (all-defined-out))
