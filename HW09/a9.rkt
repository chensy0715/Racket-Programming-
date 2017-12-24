#lang racket

;;import parenthec.rkt
(require "parenthec.rkt")

;;program counter
(define-program-counter pc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Won Yong Ha
;; woha@indiana.edu
;;
;; CSCI-C 311
;; Assignment 9 setp 6
;;
;; Start: March 22 2016
;; End:March 23 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Registerization
(define-registers v* env* x* k* a* c* k^* n*)

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

;;kt
(define-union kt
  (empty-k dismount)
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
(define-label value-of-cps
  (union-case x* expr
              ;;const
              [(const cexp)
               (begin
                 (set! v* cexp)
                 (set! pc apply-k))]
              ;;mult
              [(mult nexp1 nexp2)
               (begin
                 (set! x* nexp1)
                 (set! k* (kt_mult-outer-k env* nexp2 k*))
                 (set! pc value-of-cps))]
              ;;sub1
              [(sub1 nexp)
               (begin
                 (set! x* nexp)
                 (set! k* (kt_just-k sub1 k*))
                 (set! pc value-of-cps))]
              ;;zero
              [(zero nexp)
               (begin
                 (set! x* nexp)
                 (set! k* (kt_just-k zero? k*))
                 (set! pc value-of-cps))]
              ;;if
              [(if test conseq alt)
               (begin
                 (set! x* test)
                 (set! k* (kt_if-k env* conseq alt k*))
                 (set! pc value-of-cps))]
              ;;letcc
              [(letcc body)
               (begin
                 (set! x* body)
                 (set! env* (envr_extend-env env* k*))
                 (set! pc value-of-cps))]
              ;;throw
              [(throw kexp vexp)
               (begin
                 (set! x* kexp)
                 (set! k* (kt_throw-outer-k env* vexp))
                 (set! pc value-of-cps))]
              ;;let
              [(let exp body)
               (begin
                 (set! x* exp)
                 (set! k* (kt_let-k env* body k*))
                 (set! pc value-of-cps))]
              ;;var
              [(var n)
               (begin
                 (set! n* n)
                 (set! pc apply-env))]
              ;;lambda
              [(lambda body)
               (begin
                 (set! v* (clos_closure body env*))
                 (set! pc apply-k))]
              ;;app
              [(app rator rand)
               (begin
                 (set! x* rator)
                 (set! k* (kt_app-outer-k env* rand k*))
                 (set! pc value-of-cps))]
              ))

;;apply-k
(define-label apply-k
    (union-case k* kt
      ;;empty-k
      [(empty-k dismount)
       (dismount-trampoline dismount)]
      ;;just-k
      [(just-k a k)
       (begin
         (set! k* k)
         (set! v* (a v*))
         (set! pc apply-k))]
      ;;mult-outer-k
      [(mult-outer-k env-cps x k)
       (begin
         (set! x* x)
         (set! env* env-cps)
         (set! k* (kt_mult-inner-k v* k))
         (set! pc value-of-cps))]
      ;;mult-inner-k
      [(mult-inner-k v k)
       (begin
         (set! k* k)
         (set! v* (* v v*))
         (set! pc apply-k))]
      ;;if-k
      [(if-k env-cps conseq alt k)
       (if v*
           (begin
             (set! x* conseq)
             (set! env* env-cps)
             (set! k* k)
             (set! pc value-of-cps))
           (begin
             (set! x* alt)
             (set! k* k)
             (set! pc value-of-cps)))]
      ;;throw-outer-k
      [(throw-outer-k env-cps k-exp)
       (begin
         (set! x* k-exp)
         (set! env* env-cps)
         (set! k* (kt_throw-inner-k v* env*))
         (set! pc value-of-cps))]
      ;;throw-inner-k
      [(throw-inner-k v env-cps)
       (begin
         (set! k* v*)
         (set! pc apply-k))]
      ;;let-k
      [(let-k env-cps body k)
       (begin
         (set! x* body)
         (set! env* (envr_extend-env env-cps v*))
         (set! k* k)
         (set! pc value-of-cps))]
      ;;app-outer-k
      [(app-outer-k env-cps rand k)
       (begin
         (set! x* rand)
         (set! env* env-cps)
         (set! k* (kt_app-inner-k v* k))
         (set! pc apply-closure))]
      ;;app-inner-k
      [(app-inner-k v k)
       (begin
         (set! c* v)
         (set! a* v*)
         (set! k^* k)
         (set! pc apply-closure))]
      ))

;;apply-env
(define-label apply-env
    (union-case env* envr
                [(empty) (error 'value-of "unbound identifier")]
                [(extend-env env v)
                 (if (zero? n*)
                     (begin
                       (set! v* v)
                       (set! pc apply-k))
                     (begin
                       (set! env* env)
                       (set! n* (sub1 n*))
                       (set! pc apply-k)))]
                ))

;;apply-closure
(define-label apply-closure
    (union-case c* clos
                [(closure body env-cps)
                 (begin
                   (set! x* body)
                   (set! env* (envr_extend-env a* env-cps))
                   (set! k* k^*)
                   (set! pc value-of-cps))]
                ))

;;empty-env
(define empty-env
  (lambda ()
    (envr_empty)))
 

;;empty-k
(define empty-k
  (lambda ()
    (kt_empty-k)))


(define main 
  (begin
    (set! x* (expr_lambda (expr_const 5)))
    (set! env* (envr_empty))
    (set! pc value-of-cps)
    (mount-trampoline kt_empty-k k* pc)
    (printf "~s\n" v*)))
