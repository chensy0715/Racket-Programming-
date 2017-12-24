#lang racket

;;Won Yong Ha

;;Start: Feb 21 2016
;;End:

;;------------------------------------------------------------------
;;Standard Defined Variable

;;empty-k
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
            (error 'empty-k "You can only invoke the empty continuation once")
            (begin (set! once-only #t) v))))))

;;empty-k
#|(define empty-k
  (lambda ()
    (lambda (v) v)))|#

;;------------------------------------------------------------------
;; 1. binary-to-decimal-cps

;;binary-to-decimal
(define binary-to-decimal
  (lambda (n)
    (cond
      [(null? n) 0]
      [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))

;;binary-to-decimal
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n)
       (k 0)]
      [else
       (binary-to-decimal-cps (cdr n)
                              (lambda(x) (k (+ (* x 2)
                                               (car n)))))]
      )))

;;Testing
#|
(binary-to-decimal '())
(binary-to-decimal-cps '() (empty-k))
(binary-to-decimal '(1))
(binary-to-decimal-cps '(1) (empty-k))
(binary-to-decimal '(0 1))
(binary-to-decimal-cps '(0 1) (empty-k))
(binary-to-decimal '(1 1 0 1))
(binary-to-decimal-cps '(1 1 0 1) (empty-k))|#

;;------------------------------------------------------------------
;; 2. times-cps

;;times
(define times
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))

;;times-cps
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls)
       (k 1)]
      [(zero? (car ls))
       (k 0)]
      [else
       (times-cps (cdr ls)
                  (lambda (v) (k (* v (car ls)))))]
      )))

;;Testing
#|
(times '(1 2 3 4 5))
(times-cps '(1 2 3 4 5) (empty-k))
(times '(1 2 3 0 3))
(times-cps '(1 2 3 0 3) (empty-k))|#

;;------------------------------------------------------------------
;; 3.

;;times-cps
(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls)
       (k 1)]
      [(zero? (car ls))
       0]
      [else
       (times-cps-shortcut (cdr ls)
                           (lambda (v) (k (* v (car ls)))))]
      )))

;;Testing
#|
(times '(1 2 3 4 5))
(times-cps-shortcut '(1 2 3 4 5) (empty-k))
(times '(1 2 3 0 3))
(times-cps-shortcut '(1 2 3 0 3) (empty-k))|#

;;------------------------------------------------------------------
;; 4. plus-cps

;;plus
(define plus
  (lambda (m)
    (lambda (n)
      (+ m n))))

;;plus-cps
(define plus-cps
  (lambda (m k)
    (k (lambda (n k) (k (+ m n))))
    ))

;;Testing
#|
((plus 2) 3)
((plus-cps 2 (empty-k)) 3 (empty-k))
((plus ((plus 2) 3)) 5)
((plus-cps ((plus-cps 2 (empty-k)) 3 (empty-k)) (empty-k)) 5 (empty-k))|#

;;------------------------------------------------------------------
;; 5. remv-first-9*-cps

;;remv-first-9*
(define remv-first-9*
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (remv-first-9* (car ls)))
          (cons (car ls) (remv-first-9* (cdr ls)))]
         [else (cons (remv-first-9* (car ls)) (cdr ls))])]
      [(eqv? (car ls) '9) (cdr ls)]
      [else (cons (car ls) (remv-first-9* (cdr ls)))])))

;;remv-first-9*-cps
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
      (cond
        [(remv-first-9*-cps (car ls)
                            (lambda (x)(equal? x (car ls))))
         ;;------------result---
         (remv-first-9*-cps (cdr ls)
                            (lambda (x) (k (cons (car ls) x))))]
        ;;-----------
        [else
         (remv-first-9*-cps (car ls)
                            (lambda (x) (k (cons x (cdr ls)))))]
        )]
      [(eqv? (car ls) '9)
       (cdr ls)]
      [else
       (remv-first-9*-cps (cdr ls)
                          (lambda (x) k (cons x (car ls))))]
      )))

;;Testing
#|
(remv-first-9* '((1 2 (3) 9)))
(remv-first-9* '((1 2 (3) 9)))
(remv-first-9* '(9 (9 (9 (9)))))
(remv-first-9* '(9 (9 (9 (9)))))
(remv-first-9* '(((((9) 9) 9) 9) 9))
(remv-first-9* '(((((9) 9) 9) 9) 9))|#

;;------------------------------------------------------------------
;; 6. cons-cell-count-cps

;;cons-cell-count
(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))

;;cons-cell-count-cps
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (cdr ls) ;;cdr ls
                            (lambda (x) (cons-cell-count-cps (car ls)
                                                             (lambda (y) (k (add1 (+ x y)))))))]
      [else
       (k 0)]
      )))

;;Testing
#|
(cons-cell-count (cons (cons (cons (cons 3 '()) (cons 2 (cons 3 (cons 1 '())))) '()) '()))
(cons-cell-count-cps (cons (cons (cons (cons 3 '()) (cons 2 (cons 3 (cons 1 '())))) '()) '()) (empty-k))
|#

;;------------------------------------------------------------------
;; 7. find-cps

;;find
(define find 
  (lambda (u s)
    (let ((pr (assv u s)))
      (if pr (find (cdr pr) s) u))))

;;find-cps
(define find-cps
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr
          (find-cps (cdr pr) s k)
          (k u)))
    ))

;;Testing
#|
(find 5 '((5 . a) (6 . b) (7 . c)))
(find-cps 5 '((5 . a) (6 . b) (7 . c)) (empty-k))
(find 7 '((5 . a) (6 . 5) (7 . 6)))
(find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k))
(find 5 '((5 . 6) (9 . 6) (2 . 9)))
(find-cps 5 '((5 . 6) (9 . 6) (2 . 9)) (empty-k))|#

;;------------------------------------------------------------------
;; 8. ack-cps

;;ack
;; ack: computes the Ackermann function
;; (http://en.wikipedia.org/wiki/Ackermann_function).  Warning: if you
;; run this program with m >= 4 and n >= 2, you'll be in for a long
;; wait.

;;ack
(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))

;;ack-cps
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m)
       (k (add1 n))]
      [(zero? n)
       (ack-cps (sub1 m) 1 k)]
      [else
       (ack-cps m
                (sub1 n)
                (lambda (x) (ack-cps (sub1 m) x k)))]
      )))

;;Testing
#|
(ack 1 2) ;;4
(ack-cps 1 2 (empty-k))|#

;;------------------------------------------------------------------
;; 9. fib-cps

;;fib
(define fib
  (lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (fib n)
       (cond
	 [(zero? n) 0]
	 [(= 1 n) 1]
	 [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))

;;fib-cps
(define fib-cps
  (lambda (n k)
    ((lambda (fib-cps k)
       (fib-cps fib-cps n k))
     (lambda (fib-cps n k)
       (cond
         [(zero? n)
          (k n)]
         [(= 1 n)
          (k 1)]
         [else
          (fib-cps fib-cps;;-------------------------
                   (sub1 (sub1 n))
                   (lambda(x) (fib-cps fib-cps;;---------------------------
                                       (sub1 n)
                                       (lambda (y) (k (+ x y))))))]
         ))
     k
     )))

;;Testing
#|
(fib 1)
(fib-cps 1 (empty-k))
(fib 2)
(fib-cps 2 (empty-k))
(fib 3)
(fib-cps 3 (empty-k))
(fib 20)
(fib-cps 20 (empty-k))|#

;;------------------------------------------------------------------
;; 10. unfold-cps

;;unfold
(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
	 (if (p seed)
	     ans
	     ((h h) (g seed) (cons (f seed) ans))))))))

;;null?-cps
(define null?-cps
  (lambda (ls k)
    (k (null? ls))))

;;car-cps
(define car-cps
  (lambda (pr k)
    (k (car pr))))

;;cdr-cps
(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))

;;unfold-cps
(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h-cps k)
       (h-cps h-cps
              (lambda (x) (x seed '() k))))
     (lambda (h-cps k)
       (k (lambda (seed ans k)
            (p seed (lambda (x);;--------------------------
                      (if x
                          (k ans)
                          (h-cps h-cps;;------------------------
                                 (lambda (y);;g
                                   (f seed (lambda (z);;f
                                             (g seed (lambda (a);;seed
                                                       (y a (cons z ans) k))
                                                      ))
                                      ))
                                 ))
                      )))
          ))
     k;;----------------------------
     )))


;;(lambda (h)
;;       (lambda (seed ans)
;;	 (if (p seed)
;;	     ans
;;	     ((h h) (g seed) (cons (f seed) ans))))))))

;;Testing
#|
(unfold null? car cdr '(a b c d e))
(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))|#

;;------------------------------------------------------------------
;; 11. unify

;;empty-s
(define empty-s
  (lambda ()
    '()))

;;unify
(define unify
  (lambda (u v s)
    (cond
      ((eqv? u v) s)
      ((number? u) `((,u . ,v) . ,s))
      ((number? v) (unify v u s))
      ((pair? u)
       (if (pair? v)
	   (let ((s (unify (find (car u) s) (find (car v) s) s)))
             (if s (unify (find (cdr u) s) (find (cdr v) s) s) #f))
	   #f))
      (else #f))))

;;unify-cps
#|(define unify-cps
  (lambda (u v s k)
    (cond
      [(eqv? u v) (k s)]
      [(number? u) ]
      [(number? v) ]
      [(pair? u)]
      ))|#

;;Testing
#|
(unify 'x 5 (empty-s))
(unify 'x 5 (unify 'y 6 (empty-s)))
(unify '(x y) '(5 6) (empty-s))
(unify 'x 5 (unify 'x 6 (empty-s)))
(unify '(x x) '(5 6) (empty-s))
(unify '(1 2 3) '(x 1 2) (empty-s))
(unify 'x 'y (empty-s))|#


;;------------------------------------------------------------------
;; 12. M-cps

;;M
(define M
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))

;;M-cps
;;(lambda (n k) (k add1 n))
(define M-cps
  (lambda (f k)
    (k (lambda (ls k)
         (cond
           [(null? ls)
            (k '())]
           [else 
            (M-cps f
               (lambda (x);;(M F)
                 (x (cdr ls) (lambda (y);;(cdr ls)
                            (f (car ls) (lambda (z) ;;(car ls)
                                          (k (cons z y)))
                               ))
                        ))
               )]
           ))
       )))

;;Testing
#|
"number 12"
((M add1) '(1 2 3 4 5))
((M-cps (lambda (n k) (k (add1 n))) (empty-k)) '(1 2 3 4 5)(empty-k))|#

;;------------------------------------------------------------------
;; 13. use-of-M-cps

;;"number 13"

;;use-of-M
(define use-of-M
  ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))

;;use-of-M-cps
(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n))) (empty-k)) '(1 2 3 4 5) (empty-k)))

;;use-of-M-cps
;;(define use-of-M-cps)

;;Testing
#|
use-of-M
use-of-M-cps|#