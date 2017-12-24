#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Won Yong Ha
;; woha@indiana.edu
;;
;; CSCI-C 311
;; Assignment 7
;;
;; Start: March 22 2016
;; End: March 23 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;value-of-cps
(define value-of-cps
  (lambda (expr env-cps k)
    (match expr
      ;;const
      [`(const ,expr)
       (apply-k k expr)]
      ;;mult
      [`(mult ,x1 ,x2)
       (value-of-cps x1 env-cps
                     (mult-outer-k env-cps x2 k))]
      ;;sub1
      [`(sub1 ,x)
       (value-of-cps x env-cps
                     (just-k sub1 k))]
      ;;zero
      [`(zero ,x)
       (value-of-cps x env-cps
                     (just-k zero? k))]
      ;;if
      [`(if ,test ,conseq ,alt)
       (value-of-cps test env-cps
                     (if-k env-cps conseq alt k))]
      ;;let/cc
      [`(let/cc ,body)
       (value-of-cps body
                     (extend-env env-cps k)
                     k)]
      ;;throw
      [`(throw ,k-exp ,v-exp)
       (value-of-cps k-exp env-cps
                     (throw-outer-k env-cps v-exp))]
      ;;let
      [`(let ,e ,body)
       (value-of-cps e env-cps
                     (let-k env-cps body k))]
      ;;var
      [`(var ,expr)
       (apply-env env-cps expr k)]
      ;;lmabda
      [`(lambda ,body)
       (apply-k k
                (make-closure body env-cps))]
      ;;app
      [`(app ,rator ,rand)
       (value-of-cps rator env-cps
                     (app-outer-k env-cps rand k))]
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
                     (extend-env env-cps val)
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
    (match env
      [`(empty-env) a]
      [`(extend-env ,v ,env)
       (if (zero? a)
           (apply-k k v)
           (apply-env env (sub1 a) k))]
      )))

;;make-closure
(define make-closure
  (lambda (body env)
    `(closure ,body ,env)))

;;apply-closure
(define apply-closure
  (lambda (rator rand k)
    (match rator
      [`(closure ,body ,env-cps)
       (value-of-cps body
                     (extend-env env-cps rand)
                     k)])))

;;extend-env
(define extend-env
  (lambda (env v)
    `(extend-env ,v ,env)))

;;empty-env
(define empty-env
  (lambda ()
    `(empty-env '(error 'value-of "unbound identifier"))))
 

;;empty-k
(define empty-k
  (lambda ()
    '(empty-k)))

;;Test
(value-of-cps '(const 5) (empty-env) (empty-k))
(value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k))
(value-of-cps '(zero (const 5)) (empty-env) (empty-k))
(value-of-cps '(sub1 (const 5)) (empty-env) (empty-k))
(value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k))
(value-of-cps '(zero (sub1 (const 6))) (empty-env) (empty-k))
(value-of-cps '(if (zero (const 5)) (const 3) (mult (const 2) (const 2))) (empty-env) (empty-k))
(value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k))
(value-of-cps '(app (lambda (const 5)) (const 6)) (empty-env) (empty-k)) 
(value-of-cps '(app (lambda (var 0)) (const 5)) (empty-env) (empty-k))
(value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k))
(value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k))
(value-of-cps '(app (lambda (if (zero (var 0)) (const 4) (const 5))) (const 3)) (empty-env) (empty-k))
(value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k))
(value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k))
(value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k))
(value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k))
(value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k))
(value-of-cps '(let/cc (const 5)) (empty-env) (empty-k))
(value-of-cps '(let/cc (throw (var 0) (const 5))) (empty-env) (empty-k))
(value-of-cps '(let/cc (throw (var 0) (mult (const 5) (const 5)))) (empty-env) (empty-k))
(value-of-cps '(let/cc (throw (app (lambda (var 0)) (var 0)) (mult (const 5) (const 5)))) (empty-env) (empty-k))
(value-of-cps '(let/cc (sub1 (throw (var 0) (const 5)))) (empty-env) (empty-k))
(value-of-cps '(let/cc (throw (throw (var 0) (const 5)) (const 6))) (empty-env) (empty-k))
(value-of-cps '(let/cc (throw (const 5) (throw (var 0) (const 5)))) (empty-env) (empty-k))
(value-of-cps '(mult (const 3) (let/cc (throw (const 5) (throw (var 0) (const 5))))) (empty-env) (empty-k))
(value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
              (empty-env)
              (empty-k))
(value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
              (empty-env)
              (empty-k))
(value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                    (lambda
                        (lambda 
                            (if (zero (var 0))  
                                (const 1)
                                (app (app (var 1) (var 1)) (sub1 (var 0)))))))
              (empty-env)
              (empty-k))