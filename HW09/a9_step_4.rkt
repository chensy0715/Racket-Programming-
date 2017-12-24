#lang racket

;;import parenthec.rkt
(require "parenthec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Won Yong Ha
;; woha@indiana.edu
;;
;; CSCI-C 311
;; Assignment 9 setp 4
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

;;envr
(define-union envr
  (empty)
  (extend-env env v))

;;kt ;;new
(define-union kt
  (empty-k)
  (just-k a k)
  (mult-outer-k env-cps x k)
  (mult-inner-k v k)
  (if-k env-cps conseq alt k)
  (let-k env-cps body k)
  (throw-outer-k env-cps k-exp)
  (throw-inner-k v env-cps)
  (app-outer-k env-cps rand k)
  (app-inner-k v k))

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
                     (kt_mult-outer-k env-cps nexp2 k-cps))];;changed
      ;;sub1
      [(sub1 nexp)
       (value-of-cps nexp env-cps
                     (kt_just-k sub1 k-cps))];;changed
      ;;zero
      [(zero nexp)
       (value-of-cps nexp env-cps
                     (kt_just-k zero? k-cps))];;changed
      ;;if
      [(if test conseq alt)
       (value-of-cps test env-cps
                     (kt_if-k env-cps conseq alt k-cps))];;changed
      ;;letcc
      [(letcc body)
       (value-of-cps body
                     (envr_extend-env env-cps k-cps)
                     k-cps)]
      ;;throw
      [(throw kexp vexp)
       (value-of-cps kexp env-cps
                     (kt_throw-outer-k env-cps vexp))];;changed
      ;;let
      [(let exp body)
       (value-of-cps exp env-cps
                     (kt_let-k env-cps body k-cps))];;changed
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
                     (kt_app-outer-k env-cps rand k-cps))];;changed
      )))

;;just-k ;;changed

;;mult-outer-k ;;changed

;;mult-inner-k ;;changed

;;if-k ;;changed

;;throw-outer-k ;;changed

;;throw-inner-k ;;changed

;;let-k ;;changed

;;app-outer-k ;;changed

;;app-inner-k ;;changed
  
;;apply-k
(define apply-k
  (lambda (k val)
    (union-case k kt;;changed
      ;;empty-k
      [(empty-k);;changed
       val]
      ;;just-k
      [(just-k a k);;changed
       (apply-k k (a val))]
      ;;mult-outer-k
      [(mult-outer-k env-cps x k);;changed
       (value-of-cps x env-cps
                     (kt_mult-inner-k val k))];;changed
      ;;mult-inner-k
      [(mult-inner-k v k);;changed
       (apply-k k (* v val))]
      ;;if-k
      [(if-k env-cps conseq alt k);;changed
       (if val
           (value-of-cps conseq env-cps k)
           (value-of-cps alt env-cps k))]
      ;;throw-outer-k
      [(throw-outer-k env-cps k-exp);;changed
       (value-of-cps k-exp env-cps
                     (kt_throw-inner-k val env-cps))];;changed
      ;;throw-inner-k
      [(throw-inner-k v env-cps);;changed
       (apply-k v val)]
      ;;let-k
      [(let-k env-cps body k);;changed
       (value-of-cps body
                     (envr_extend-env env-cps val)
                     k)]
      ;;app-outer-k
      [(app-outer-k env-cps rand k);;changed
       (value-of-cps rand env-cps
                     (kt_app-inner-k val k))];;changed
      ;;app-inner-k
      [(app-inner-k v k);;changed
       (apply-closure v val k)]
      )))

;;apply-env
(define apply-env
  (lambda (env a k)
    (union-case env envr
                [(empty) (error 'value-of "unbound identifier")]
                [(extend-env env v)
                 (if (zero? a)
                     (apply-k k v)
                     (apply-env env (sub1 a) k))]
                )))

;;apply-closure
(define apply-closure
  (lambda (rator rand k)
    (union-case rator clos
                [(closure body env-cps)
                 (value-of-cps body
                               (envr_extend-env env-cps rand)
                               k)])))

;;extend-env
(define extend-env
  (lambda (env v)
    `(extend-env ,v ,env)))

;;empty-env
(define empty-env
  (lambda ()
    (envr_empty)))
 

;;empty-k
(define empty-k
  (lambda ()
    (kt_empty-k)));;changed

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