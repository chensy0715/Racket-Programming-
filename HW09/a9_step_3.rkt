#lang racket

;;import parenthec.rkt
(require "parenthec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Won Yong Ha
;; woha@indiana.edu
;;
;; CSCI-C 311
;; Assignment 9 setp 3
;;
;; Start: March 22 2016
;; End:March 23 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;expr
(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))

;;clos
(define-union clos
  (closure body env-cps))

;;envr ;;new
(define-union envr
  (empty)
  (extend-env env v))

;;value-of-cps
(define value-of-cps
  (lambda (exp env-cps k-cps)
    (union-case exp expr
      ;;const
      [(const cexp)
       (apply-k k-cps cexp)]
      ;;mult
      [(mult nexp1 nexp2)
       (value-of-cps nexp1 env-cps
                     (mult-outer-k env-cps nexp2 k-cps))]
      ;;sub1
      [(sub1 nexp)
       (value-of-cps nexp env-cps
                     (just-k sub1 k-cps))]
      ;;zero
      [(zero nexp)
       (value-of-cps nexp env-cps
                     (just-k zero? k-cps))]
      ;;if
      [(if test conseq alt)
       (value-of-cps test env-cps
                     (if-k env-cps conseq alt k-cps))]
      ;;letcc
      [(letcc body)
       (value-of-cps body
                     (envr_extend-env env-cps k-cps);;changed
                     k-cps)]
      ;;throw
      [(throw kexp vexp)
       (value-of-cps kexp env-cps
                     (throw-outer-k env-cps vexp))]
      ;;let
      [(let exp body)
       (value-of-cps exp env-cps
                     (let-k env-cps body k-cps))]
      ;;var
      [(var n)
       (apply-env env-cps n k-cps)]
      ;;lmabda
      [(lambda body)
       (apply-k k-cps
                (clos_closure body env-cps))]
      ;;app
      [(app rator rand)
       (value-of-cps rator env-cps
                     (app-outer-k env-cps rand k-cps))]
      )))

;;just-k
(define just-k
  (lambda (a^ k^)
    `(just-k ,a^ ,k^)))

;;mult-outer-k
(define mult-outer-k
  (lambda (env-cps^ x2^ k^)
    `(mult-outer-k ,env-cps^ ,x2^ ,k^)))

;;mult-inner-k
(define mult-inner-k
  (lambda (v^ k^)
    `(mult-inner-k ,v^ ,k^)))

;;if-k
(define if-k
  (lambda (env-cps^ conseq^ alt^ k^)
    `(if-k ,env-cps^ ,conseq^ ,alt^ ,k^)))

;;throw-outer-k
(define throw-outer-k
  (lambda (env-cps^ k-exp^)
    `(throw-outer-k ,env-cps^ ,k-exp^)))

;;throw-inner-k
(define throw-inner-k
  (lambda (v^ env-cps^)
    `(throw-inner-k ,v^ ,env-cps^)))

;;let-k
(define let-k
  (lambda (env-cps^ body^ k^)
    `(let-k ,env-cps^ ,body^ ,k^)))

;;app-outer-k
(define app-outer-k
  (lambda (env-cps^ rand^ k^)
    `(app-outer-k ,env-cps^ ,rand^ ,k^)))

;;app-inner-k
(define app-inner-k
  (lambda (v^ k^)
    `(app-inner-k ,v^ ,k^)))

;;apply-k
(define apply-k
  (lambda (k val)
    (match k
      ;;empty-k
      [`(empty-k)
       val]
      ;;just-k
      [`(just-k ,a ,k)
       (apply-k k (a val))]
      ;;mult-outer-k
      [`(mult-outer-k ,env-cps ,x ,k)
       (value-of-cps x env-cps
                     (mult-inner-k val k))]
      ;;mult-inner-k
      [`(mult-inner-k ,v ,k)
       (apply-k k (* v val))]
      ;;if-k
      [`(if-k ,env-cps ,conseq ,alt ,k)
       (if val
           (value-of-cps conseq env-cps k)
           (value-of-cps alt env-cps k))]
      ;;throw-outer-k
      [`(throw-outer-k ,env-cps ,k-exp)
       (value-of-cps k-exp env-cps
                     (throw-inner-k val env-cps))]
      ;;throw-inner-k
      [`(throw-inner-k ,v ,env-cps)
       (apply-k v val)]
      ;;let-k
      [`(let-k ,env-cps ,body ,k)
       (value-of-cps body
                     (envr_extend-env env-cps val);;changed
                     k)]
      ;;app-outer-k
      [`(app-outer-k ,env-cps ,rand ,k)
       (value-of-cps rand env-cps
                     (app-inner-k val k))]
      ;;app-inner-k
      [`(app-inner-k ,v ,k)
       (apply-closure v val k)]
      )))

;;apply-env
(define apply-env
  (lambda (env a k)
    (union-case env envr
                [(empty) (error 'value-of "unbound identifier")];;changed
                [(extend-env env v);;changed--wowwwwwwwwwwwwwwwwwwwwwwww OMG TT OTL
                 (if (zero? a)
                     (apply-k k v)
                     (apply-env env (sub1 a) k))]
                )))

;;make-closure ;;changed

;;apply-closure
(define apply-closure
  (lambda (rator rand k)
    (union-case rator clos;;changed
                [(closure body env-cps);;changed
                 (value-of-cps body
                               (envr_extend-env env-cps rand);;changed
                               k)])))

;;extend-env
(define extend-env
  (lambda (env v)
    `(extend-env ,v ,env)))

;;empty-env
(define empty-env
  (lambda ()
    (envr_empty)));;changed
 

;;empty-k
(define empty-k
  (lambda ()
    '(empty-k)));;changed

;;main
(define main 
  (lambda ()
    (value-of-cps 
     (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (empty-env)
     (empty-k))))

;;Operate
(main);;120