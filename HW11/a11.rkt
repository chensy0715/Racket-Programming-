#lang racket
(require "mk.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Won Yong Ha
;;
;; CSCI-C 311
;; Assignment 11
;;
;; Start: 12 April 2016
;; End: 13 April 2016
;;
;; Comments:
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; apply-Go
(define apply-Go
  (lambda (G e t)
    (fresh (a G^)
      (== `(,a . ,G^) G)
      (fresh (aa da)
        (== `(,aa . ,da) a)
        (conde
          ((== aa e) (== da t))
          ((=/= aa e) (apply-Go G^ e t)))))))

;; !-
(define !-
  (lambda (G e t)
    (conde
      ((numbero e) (== 'Nat t))
      ((== t 'Bool)
       (conde
         ((== #t e))
         ((== #f e))))
      ((fresh (ne1 ne2)
         (== `(+ ,ne1 ,ne2) e)
         (== 'Nat t)
         (!- G ne1 'Nat)
         (!- G ne2 'Nat)))
      ((fresh (teste anse elsee)
        (== `(if ,teste ,anse ,elsee) e)
        (!- G teste 'Bool)
        (!- G anse t)
        (!- G elsee t)))
      ((symbolo e) (apply-Go G e t))
      ((fresh (x b)
        (== `(lambda (,x) ,b) e)
        (symbolo x)
        (fresh (tx tb)          
          (== `(,tx -> ,tb) t)
          (!- `((,x . ,tx) . ,G) b tb))))
      ((fresh (e1 arg)
        (== `(,e1 ,arg) e)
        (fresh (targ)
          (!- G e1 `(,targ -> ,t))
          (!- G arg targ))))
      ;;zero?
      ((fresh (x)
              (== `(zero? ,x) e)
              (== 'Bool t)
              (!- G x 'Nat)))
      ;;sub1
      ((fresh (x)
              (== `(sub1 ,x) e)
              (== `Nat t)
              (!- G x 'Nat)))
      ;;not
      ((fresh (x)
              (== `(not ,x) e)
              (== 'Bool t)
              (!- G x 'Bool)))
      ;;*
      ((fresh (x1 x2)
              (== `(* ,x1 ,x2) e)
              (== 'Nat t)
              (!- G x1 'Nat)
              (!- G x2 'Nat)))
      ;;fix ;;Figure together with Nicholas Palumbo
      ((fresh (x)
              (== `(fix ,x) e)
              (!- G x `(,t -> ,t))))
      )))


;; Testing
#|
(run* (q) (!- '() #t q))
(run* (q) (!- '() 17 q))
(run* (q) (!- '() '(zero? 24) q))
(run* (q) (!- '() '(zero? (sub1 24)) q))
(run* (q) (!- '() '(not (zero? (sub1 24))) q))
(run* (q) (!- '() '(zero? (sub1 (sub1 18))) q))
(run* (q) (!- '()  '(lambda (n) (if (zero? n) n n)) q));'((Nat -> Nat))
(run* (q) (!- '() '((lambda (n) (zero? n)) 5) q))
(run* (q) (!- '() '(if (zero? 24) 3 4) q))
(run* (q) (!- '() '(if (zero? 24) (zero? 3) (zero? 4)) q))
(run* (q) (!- '() '(lambda (x) (sub1 x)) q))
(run* (q) (!- '() '(lambda (a) (lambda (x) (+ a x))) q))
(run* (q) (!- '() '(lambda (f) (lambda (x) ((f x) x))) q))
(run* (q) (!- '() '(sub1 (sub1 (sub1 6))) q))
(run 1 (q) (fresh (t) (!- '() '(lambda (f) (f f)) t)));'()
(length (run 20 (q) (fresh (lam a b) (!- '() `((,lam (,a) ,b) 5) 'Nat) (== `(,lam (,a) ,b) q))))
(length (run 30 (q) (!- '() q 'Nat)))
(length (run 30 (q) (!- '() q '(Nat -> Nat))))
(length (run 500 (q) (!- '() q '(Nat -> Nat))))
(length (run 30 (q) (!- '() q '(Bool -> Nat))))
(length (run 30 (q) (!- '() q '(Nat -> (Nat -> Nat)))))
(length (run 100 (q) (fresh (e t) (!- '() e t) (== `(,e ,t) q))))
(length (run 100 (q) (fresh (g e t) (!- g e t) (== `(,g ,e ,t) q))))
(length (run 100 (q) (fresh (g v) (!- g `(var ,v) 'Nat) (== `(,g ,v) q))))
(run 1 (q)
       (fresh (g)
	 (!- g
	      '((fix (lambda (!)
		       (lambda (n)
			 (if (zero? n)
			     1
			     (* n (! (sub1 n)))))))
		5)
	      q)))
(run 1 (q)
       (fresh (g)
	 (!- g
	      '((fix (lambda (!)
		       (lambda (n)
			 (* n (! (sub1 n))))))
		5)
	      q)))

;; Master Testing
|#
;;(require "a11-student-tests.rkt")
;;(test-file #:file-name "a11.rkt")