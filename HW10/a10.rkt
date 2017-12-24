#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Won Yong Ha
;;
;; CSCI-C 311
;; Assignment 10
;;
;; Start: March 27 2016
;; End: March 30 2016
;;
;;
;; Comments
;; Well a10-student-tests.rkt is not working. Plz check the file
;; one more time. From the code:
;; (run* (q) (assoco 'x '((y . 6) (x . 5)) q))
;; This example was not working I dont know why
;; This example was only one not working
;; But the test says it is correct.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Import
(require "mk.rkt")
(require "numbers.rkt")

;;-----------------------------------------------------------------
;;Part I

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 

#|
(run 2 (q)
  (== 5 q)
  (conde
   [(conde 
     [(== 5 q)
      (== 6 q)])
    (== 5 q)]
   [(== q 5)]))
|#

;; A: '(5)

;; 2 What is the value of

#|
(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))
|#

;; A: '(((_.0 _.1) (=/= ((_.0 tag))) (sym _.0) (absento (tag _.1))))

;; 3 What do the following miniKanren constraints mean?
;; a ==       : Be equal and have to be eqaul
;; b =/=      : Not be euqal and have to be not equal
;; c absento  : Not in the clauses
;; d numbero  : Be number and have to be number
;; e symbolo  : Be symbol and have to be symbol

;;-----------------------------------------------------------------
;;Part II

;;Example
(define assoc
  (lambda (x ls)
    (match-let* ((`(,a . ,d) ls)
                 (`(,aa . ,da) a))
      (cond
        ((equal? aa x) a)
        ((not (equal? aa x)) (assoc x d))))))

(define reverse
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else
       (match-let* ((`(,a . ,d) ls)
                    (res (reverse d)))
         (append res `(,a)))))))

(define stutter
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else 
        (match-let* ((`(,a . ,d) ls)
		     (res (stutter d)))
          `(,a ,a . ,res))))))

;;----------------------------------------
;; Part II Answers

#|
;;Example
(define assoc
  (lambda (x ls)
    (match-let* ((`(,a . ,d) ls)
                 (`(,aa . ,da) a))
      (cond
        ((equal? aa x) a)
        ((not (equal? aa x)) (assoc x d))))))
|#

;;assoco
(define assoco
  (lambda (x ls o)
    (fresh (a d)
           (== `(,a . ,d) ls)
           (fresh (aa da)
                  (== `(,aa . ,da) a)
                  (conde
                   [(== aa x)
                    (== a o)]
                   [(=/= aa x) ;;wow this is wield expression
                    ;;But not necessary ^^ -> well its necessay Wow
                    (fresh (res)
                           (assoco x d res))]
                   ))
           )))

#|
(define reverse
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else
       (match-let* ((`(,a . ,d) ls)
                    (res (reverse d)))
         (append res `(,a)))))))
|#

;;recerseo
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

#|
;;Example
(define stutter
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else 
        (match-let* ((`(,a . ,d) ls)
		     (res (stutter d)))
          `(,a ,a . ,res))))))
|#

;;stuttero (From Lab Time -Lab Quiz-)
(define stuttero
  (lambda (ls o)
    (conde
     [(== ls '()) (== o '())]
     [(fresh (a d res)
             (== `(,a . ,d) ls)
             (== `(,a ,a . ,res) o)
             (stuttero d res))]
     )))



;;Testing
#|
(run 1 (q) (stuttero q '(1 1 2 2 3 3)))
(run* (q) (stuttero q '(1 1 2 2 3 3)))
(run 1 (q) (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero a `(1 ,b ,c 2 3 ,d))))
(run 1 (q) (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero `(,b 1) `(,c . ,d))))
(run 1 (q) (fresh (e f g) (== q `(,e ,f ,g)) (stuttero `(,e . ,f) g)))
(run 2 (q) (fresh (e f g) (== q `(,e ,f ,g)) (stuttero `(,e . ,f) g)))
(run* (q) (assoco 'x '() q))
(run* (q) (assoco 'x '((x . 5)) q))
(run* (q) (assoco 'x '((y . 6) (x . 5)) q))
(run* (q) (assoco 'x '((x . 6) (x . 5)) q))
(run* (q) (assoco 'x '((x . 5)) '(x . 5)))
(run* (q) (assoco 'x '((x . 6) (x . 5)) '(x . 6)))
(run* (q) (assoco 'x '((x . 6) (x . 5)) '(x . 5)))
(run* (q) (assoco q '((x . 6) (x . 5)) '(x . 5)))
(run* (q) (assoco 'x '((x . 6) . ,q) '(x . 6)))
(run 5 (q) (assoco 'x q '(x . 5)))
(run 5 (q) (fresh (x y z)
                (assoco x y z)
                (== `(,x ,y ,z) q)))
(run* (q) (reverseo '() q))
(run* (q) (reverseo '(a) q))
(run* (q) (reverseo '(a b c d) q))
(run* (q) (fresh (x) (reverseo `(a b ,x c d) q)))
(run* (x) (reverseo `(a b ,x d) '(d c b a)))
(run* (x) (reverseo `(a b c d) `(d . ,x)))
(run* (q) (fresh (x) (reverseo `(a b c d) `(d . (,q . ,x)))))
(run 10 (q) (fresh (x y) (reverseo x y) (== `(,x ,y) q)))
|#

;;Master Testing
;;(require "a10-student-tests.rkt")
;;(test-file #:file-name "a10.rkt")