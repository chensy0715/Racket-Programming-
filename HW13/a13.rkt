#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Won Yong Ha
;;
;; CSCI-C 311
;; Assignment 13
;;
;; Start: 25 April 2016
;; End: 27 April 2016
;;
;; Comments:
;; This is too hard!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; "fo-val" should be optional assignment!! TT
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; import
(require "mk.rkt")
(require "numbers.rkt")


;;-------------------------------------------------------------------------
;; 1. listo: helped by Nick Palumbo

;;listo
(define listo
  (lambda (als)
    (conde
     [(== als empty)]
     [(fresh (x y)
             (== `(,x . ,y) als)
             (listo y))]
     )))

;;Testing
#|
(run 1 (q) (listo '(a b c d e)))
(run 1 (q) (listo '(a b c d . e)))
(run 4 (q) (listo q))
(run 4 (q) (listo `(a b ,q)))
|#


;;-------------------------------------------------------------------------
;; 2. facto

;;facto
(define facto
  (lambda (n1 n2) 
    (conde
      [(== n1 empty) (== n2 '(1))]
      [(fresh (n1_f n2_f)
              (facto n1_f n2_f)
              (minuso n1 '(1) n1_f)
              (*o n1 n2_f n2))]
      )))

;;Testing
#|
(run 1 (q) (facto  q '(0 0 0 1 1 1 1)))
(run 1 (q) (facto (build-num 5) q))
(run 6 (q) (fresh (n1 n2) (facto n1 n2) (== `(,n1 ,n2) q)))
|#


;;-------------------------------------------------------------------------
;; 3. fibs

;;fibs
(define fibs
  (lambda (n)
    (cond
      ((eqv? n 0) (values 1 1))
      (else
       (let ((n- (- n 1)))
         (let-values (((u v) (fibs n-)))
           (let ((u+v (+ u v)))
             (values v u+v))))))))

;;Testing
#|
(fibs 0)
(fibs 1)
(fibs 2)
(fibs 3)
|#

;;fibso
(define fibso
  (lambda (n o1 o2)
    (conde
     [(== n empty) (== o1 '(1))
                   (== o2 '(1))]
     [(fresh (n_f o1_f o2_f)
             (minuso n '(1) n_f)
             (fibso n_f o1_f o2_f)
             (pluso o1_f o2_f o2)
             (== o1 o2_f)
             )]
     )))

;;Testing
#|
(run 1 (a b) (fibso '() a b));;'(((1) (1)))
(run 4 (q) 
     (fresh (n o1 o2) 
            (== q `(,n ,o1 ,o2)) 
            (fibso n o1 o2)))
(run 1 (q) 
     (fresh (n o1) 
            (== q `(,n ,o1))
            (fibso n o1 (build-num 5))))
(run 1 (q) 
     (fresh (n o2) 
            (== q `(,n ,o2))
            (fibso n (build-num 5) o2)))
|#


;;-------------------------------------------------------------------------
;; 4.

(require (except-in (rename-in racket (eval J-eval)) ==))

;;reverseo
(define reverseo
  (lambda (ls o)
    (conde
     [(== '() ls)
      (== o '())]
     [(fresh (a d)
             (== `(,a . ,d) ls)
             (fresh (res)
                    (reverseo d res)
                    (appendo res `(,a) o)))]
     )))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define pairo
  (lambda (p)
    (fresh (a d)
           (conso a d p))))

;; NB, there's no base case.
(define (lookup x vars vals o)
  (fresh (y vars^ a vals^)
    (== `(,y . ,vars^) vars)
    (== `(,a . ,vals^) vals)
    (conde
      ((== x y) (== o a))
      ((=/= x y) (lookup x vars^ vals^ o)))))

;;-------------------------------------------------------
;;val-ofo

;;valof*
(define (valof* exps vars vals o)
  (conde
    ((== `() exps) (== o `()))
    ((fresh (exp exps^)
       (== exps `(,exp . ,exps^))
       (fresh (v v^)
         (== o `(,v . ,v^))
         (valof exp vars vals v)
         (valof* exps^ vars vals v^))))))

;;valof
(define (valof exp vars vals o)
  (conde
;;  ((numbero exp) (== o exp))
    ((symbolo exp) (lookup exp vars vals o))
    ((== exp `(quote ,o))
     (absento 'closure o)
     (absento 'quote vars))
    ((fresh (exps)
       (== exp `(list . ,exps))
       (absento 'list vars)
       (valof* exps vars vals o)))
    ((fresh (x b)
       (== exp `(λ (,x) ,b))
       (absento 'λ vars)
       (symbolo x)
       (== o `(closure ,x ,b ,vars ,vals))))
    ((fresh (rator rand)
       (== exp `(,rator ,rand))
      (fresh (x b vars^ vals^ a)
        (valof rator vars vals `(closure ,x ,b ,vars^ ,vals^))
        (valof rand vars vals a)
        (valof b `(,x . ,vars^) `(,a . ,vals^) o))))))

;;-------------------------------------------------------
;;fo-lavo collaborate with Nick Palumbo

;;fo-lavo*
(define (fo-lavo* exps vars vals o)
  (conde
    ((== `() exps) (== o `()))
    ((fresh (exp exps^)
       (== exps `(,exp . ,exps^))
       (fresh (v v^)
         (== o `(,v . ,v^))
         (fo-lavo exp vars vals v)
         (fo-lavo* exps^ vars vals v^))))))

;;fo-lavo
(define (fo-lavo exp vars vals o)
  (conde
    ((symbolo exp) (lookup exp vars vals o))
    ((== exp `(,o etouq))
     (absento 'closure o)
     (absento  'etouq vars))
    ((fresh (x b)
       (== exp `(,b (,x) adbmal))
       (absento 'adbmal vars)
       (symbolo x)
       (== o `(closure ,x ,b ,vars ,vals))))
    ((fresh (exps re)
            (reverseo exp re)
            (== re `(tsil . ,exps))
            (absento 'tsil vars)
            ;;(appendo exps 'tsil exps)
            (fo-lavo* exps vars vals o)))
    ((fresh (rator rand)
       (== exp `(,rand ,rator))
      (fresh (x b vars^ vals^ a)
        (fo-lavo rand vars vals `(closure ,x ,b ,vars^ ,vals^))
        (fo-lavo rator vars vals a)
        (fo-lavo b `(,x . ,vars^) `(,a . ,vals^) o))))
    ))

;;Testing
#|
(run 1 (q) (fo-lavo '(etouq etouq) '() '() q))
(run 1 (q) (fo-lavo '((cat etouq) tsil) '() '() q))
(run 1 (q) (fo-lavo '((cat etouq) . tsil) '() '() q))
(run 1 (q) (fo-lavo '((dog etouq) (cat etouq) tsil) '() '() q))
(run 1 (q) (fo-lavo q '() '() q))
|#


;;-------------------------------------------------------------------------
;; 5. color-middle-earth

;;middle-earth
(define middle-earth
  '((lindon eriador forodwaith)
    (forodwaith lindon rhovanion eriador)
    (eriador lindon forodwaith rhovanion enedwaith)
    (rhovanion forodwaith eriador enedwaith rohan rhun)
    (enedwaith eriador rhovanion rohan gondor)
    (rohan enedwaith rhovanion rhun gondor mordor)
    (gondor enedwaith rohan mordor)
    (rhun rohan rhovanion khand mordor)
    (mordor gondor rohan rhun khand harad)
    (khand mordor rhun harad)
    (harad mordor khand)))

;;color-middle-earth


;;Testing
#|
(color-middle-earth '(red orange purple black))
|#

;;-------------------------------------------------------------------------
;;Master Testing
#|
(require "a13-student-tests.rkt")
(test-file #:file-name "a13.rkt")
|#