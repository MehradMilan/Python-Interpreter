#lang racket
(require (lib "eopl.ss" "eopl"))

(define-datatype program program?
  (prog
   (program statements?)))

(define-datatype statements statements?
  (stmt
   (stmt statement?))
  (stmts
   (stmts statements?)
   (stmt statement?)))

(define-datatype statement statement?
  (stmt-compound-stmt
   (stmt compound-stmt?))
  (stmt-simple-stmt
   (stmt simple-stmt?)))

(define-datatype simple-stmt simple-stmt?
  (simple-stmt-assignment
   (assignment assignments?))
  (simple-stmt-global-stmt
   (stmt global-stmt?))
  (simple-stmt-return-stmt
   (stmt return-stmt?))
  (simple-stmt-pass)
  (simple-stmt-break)
  (simple-stmt-continue)
  (simple-stmt-print-stmt 
   (stmt (list-of atom-exp?))))

(define-datatype compound-stmt compound-stmt?
  (compound-stmt-function-def 
   (stmt function-def?))
  (compound-stmt-if-stmt 
   (stmt if-stmt?))
  (compound-stmt-for-stmt 
   (stmt for-stmt?)))

(define-datatype assignments assignments?
  (assignment-stmt 
   (id symbol?)
   (exp1 expression?)))

(define-datatype return-stmt return-stmt?
  (void-return-stmt)
  (value-return-stmt
   (exp1 expression?)))

(define-datatype global-stmt global-stmt?
  (global-stmt1 
   (id symbol?)))

(define-datatype function-def function-def?
  (function-def-with-params
   (id symbol?)
   (params params?)
   (stmts statements?)
   (saved-env environment?))
  (funcion-def-without-params
   (id symbol?)
   (stmts statements?)
   (saved-env environment?)))

(define-datatype params params?
  (param 
   (params params-with-default?))
  (params1
   (params-exp params?)
   (param-exp params-with-default?)))

(define-datatype params-with-default params-with-default?
  (param-with-default
   (id symbol?)
   (exp1 expression?)))

(define-datatype if-stmt if-stmt?
  (if-stmt1
   (exp1 expression?)
   (stmts statements?)
   (else-block else-block?)))

(define-datatype else-block else-block?
  (else-block1
   (stmts statements?)))

(define-datatype for-stmt for-stmt?
  (for-stmt1 
   (id symbol?)
   (exp1 expression?)
   (stmts statements?)))

(define-datatype expression expression?
  (expression1
   (exp1 disjunction-exp?)))

(define-datatype disjunction-exp disjunction-exp?
  (cnj-disj
   (exp conjunction-exp?))
  (or-exp
   (dis-exp disjunction-exp?)
   (con-exp conjunction-exp?)))

(define-datatype conjunction-exp conjunction-exp?
  (inv-conj
   (exp inversion-exp?))
  (and-exp
   (con-exp conjunction-exp?)
   (inv-exp inversion-exp?)))

(define-datatype inversion-exp inversion-exp?
  (not-exp
   (inv-exp inversion-exp?))
  (comparison-inv
   (cmp-exp comparison-exp?)))

(define-datatype comparison-exp comparison-exp?
  (eq-sum-comparison
   (exp eq-sum-exp?))
  (lt-sum-comparison
   (exp lt-sum-exp?))
  (gt-sum-comparison
   (exp gt-sum-exp?))
  (sum-comparison
   (exp sum-exp?)))

(define-datatype eq-sum-exp eq-sum-exp?
  (eq
   (exp1 sum-exp?)
   (exp2 sum-exp?)))

(define-datatype lt-sum-exp lt-sum-exp?
  (lt
   (exp1 sum-exp?)
   (exp2 sum-exp?)))

(define-datatype gt-sum-exp gt-sum-exp?
  (gt
   (exp1 sum-exp?)
   (exp2 sum-exp?)))

(define-datatype sum-exp sum-exp?
  (sum
   (exp1 sum-exp?)
   (exp2 term-exp?))
  (minus 
   (exp1 sum-exp?)
   (exp2 term-exp?))
  (term-sum
   (exp1 term-exp?)))

(define-datatype term-exp term-exp?
  (mul-term 
   (exp1 term-exp?)
   (exp2 factor-exp?))
  (div-term 
   (exp1 term-exp?)
   (exp2 factor-exp?))
  (term
   (exp1 factor-exp?)))

(define-datatype factor-exp factor-exp?
  (plus-factor 
   (exp1 power-exp?))
  (minus-factor 
   (exp1 power-exp?))
  (power-factor 
   (exp1 power-exp?)))

(define-datatype power-exp power-exp?
  (power1
   (atom atom-exp?)
   (factor factor-exp?))
  (primary-power
   (exp1 primary-exp?)))

(define-datatype primary-exp primary-exp?
  (atom-primary
   (exp1 atom-exp?))
  (array-primary
   (prim primary-exp?)
   (exp expression?))
  (callable-primary 
   (prim primary-exp?))
  (callable-primary-with-args
   (prim primary-exp?)
   (args arguments-exp?)))

(define-datatype arguments-exp arguments-exp?
  (arguments1
   (exp1 expression?))
  (arguments2 
   (args arguments-exp?)
   (exp1 expression?)))

(define-datatype atom-exp atom-exp?
  (id-atom
   (id symbol?))
  (bool-atom
   (val boolean?))
  (none-atom)
  (num-atom
   (val number?))
  (list-atom
   (val list-exp?)))

(define-datatype list-exp list-exp?
  (list1
   (exps expressions?))
  (empty-list))

(define-datatype expressions expressions?
  (exps
   (exps expressions?)
   (exp1 expression?))
  (a-exp
   (exp1 expression?)))

;Todo: Add all values
(define-datatype expval expval?
  (num-val
   (num number?))
  (empty-val))

(define-datatype environment environment?
  (empty-environment)
  (extend-environment
   (var string?)
   (val expval?)
   (env environment)))

(provide (all-defined-out))
(#%provide (all-defined))
