#lang racket

(require "passes/parser.rkt")
(require "passes/lexer.rkt")

(define (parse-scan prog-string)
  (python-parser (lex-this prog-string))
  )

(define (evaluate file-name)
  (parse-scan (string-join (file->lines file-name)))
  )

(evaluate "tests/test.py")

(provide (all-defined-out))



;;; (list 
;;; (func "function" (empty-param) (list (pass)))
;;;  (for_stmt "i" (function_call (ref "range") (expressions (atomic_num_exp 10) (empty-expr)))
;;;   (list (assign "x" (binary_op #<procedure:...r/passes/parser.rkt:74:37> (ref "i") (atomic_num_exp 2))) (assign "x" (binary_op #<procedure:...r/passes/parser.rkt:74:37> (ref "x") (atomic_num_exp 2)))))
;;;    (for_stmt "i" (atomic_list_exp (expressions (atomic_num_exp 3) (expressions (atomic_num_exp 2) (empty-expr)))) (list (assign "z" (atomic_num_exp 1)) (break))) (assign "i" (atomic_num_exp 2)) (if_stmt (binary_op #<procedure:equal?> (ref "i") (atomic_num_exp 2)) (list (pass)) (list (break))))