#lang racket
(require "monads.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Won Yong Ha
;;
;; CSCI-C 311
;; Assignment 12
;;
;; Start: 21 April 2016
;; End: 21 April 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;---------------------------------------------------------
;; Maybe Monad

;;-----------------------------------------
;; 1. assv-maybe (From the lab)
(define (assv-maybe x als)
  (cond
    [(null? als) (fail)]
    [(eqv? x (car (car als)))
     (return-maybe (cdar als))]
    [else (assv-maybe x (cdr als))]
    ))

;;Testing
#|
(assv-maybe 'c '((a . 1) (b . 2) (c . 3)))
(assv-maybe 'd '((a . 1) (b . 2) (c . 3)))
|#


;;---------------------------------------------------------
;; Writer Monad

;;----------------------a12-student-tests.rkt-------------------
;; 2. partition-writer
(define (partition-writer fn als)
  (cond
    [(null? als) (return-writer '())]
    [(fn (car als))
     (bind-writer
      (tell-writer (car als))
      (lambda (x)
        (partition-writer fn (cdr als))))]
    [else
      (bind-writer
       (return-writer (car als))
       (lambda (x)
         `(,(cons x (car (partition-writer fn (cdr als)))) . ,(cdr (partition-writer fn (cdr als))))))]
    ))

;;Testing
#|
(partition-writer even? '(1 2 3 4 5 6 7 8 9 10))
(partition-writer odd? '(1 2 3 4 5 6 7 8 9 10))
|#

;;-----------------------------------------
;; 3. powerXpartials

;;power
(define power
  (lambda (x n)
    (cond
      [(zero? n) 1]
      [(= n 1) x]
      [(odd? n) (* x (power x (sub1 n)))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (let ((y (power x nhalf)))
                     (* y y)))])))

;;powerXpartials
(define (powerXpartials x n)
  (cond
    [(zero? n)
     (return-writer 0)]
    [(= n 1)
     (return-writer x)]
    [(odd? n)
     (bind-writer
      (powerXpartials x (- n 1))
      (lambda (a)
        (bind-writer
         (tell-writer a)
         (lambda (b)
           (return-writer (* x a))))))]
    [(even? n)
     (bind-writer
      (powerXpartials x (/ n 2))
      (lambda (a)
        (bind-writer
         (tell-writer a)
         (lambda (b)
           (return-writer (* a a))))))]
    ))

;;Testing
#|
(powerXpartials 2 6)
(powerXpartials 3 5)
(powerXpartials 5 7)
|#


;;---------------------------------------------------------
;; State Monad

;;-----------------------------------------
;; 4. replace-with-count ;;Helped by Kellen Adolf
(define (replace-with-count x als)
  (cond
    [(null? als)
     (return-state '())]
    [(eqv? x (car als))
     (bind-state
      (replace-with-count x (cdr als))
      (lambda (a)
        (lambda (b)
          `((,b ,a) . ,(add1 b)))))]
    [(pair? (car als))
     (bind-state
      (replace-with-count x (car als))
      (lambda (a)
        (bind-state
         (replace-with-count x (cdr als))
         (lambda (c)
           (return-state (cons a c))))))]
    [else
     (bind-state
      (replace-with-count x (cdr als))
      (lambda (a)
        (return-state (cons (car als) a))))]
    ))

;;Testing
#|
((replace-with-count 'o '(a o (t o (e o t ((n) o))))) 0);;((a 0 (t 1 (e 2 t ((n) 3)))) . 4)
((replace-with-count 'o '((h (i s o) a) o s (o e n))) 0);;(((h (i s 0) a) 1 s (2 e n)) . 3)
((replace-with-count 'o '(o (h (o s o) o) o)) 0);;((0 (h (1 s 2) 3) 4) . 5)
|#

;;---------------------------------------------------------
;; Mixed Monads Problems

(define traverse
  (lambda (return bind f)
    (letrec
        ((trav
          (lambda (tree)
            (cond
              [(pair? tree)
               (do bind
                 (a <- (trav (car tree)))
                 (d <- (trav (cdr tree)))
                 (return (cons a d)))]
              [else (f tree)]))))
      trav)))

;;-----------------------------------------
;; 5. reciprocal
(define (reciprocal x)
  (cond
    [(zero? x)
     (fail)]
    [else
     (return-maybe (/ 1 x))]
    ))

;;Testing
#|
(reciprocal 0)
(reciprocal 2)
|#

;;-----------------------------------------
;; 6. halve
(define (halve x)
  (cond
    [(even? x)
     (return-writer (/ x 2))]
    [else
     (bind-writer
      (tell-writer x)
      (lambda (a)
        (return-writer x)))]
    ))

;;Testing
#|
(define traverse-halve
  (traverse return-writer bind-writer halve))
(traverse-halve '((1 . 2) . (3 . (4 . 5))));;(((1 . 1) . (3 . (2 . 5))) . (1 3 5))
|#

;;-----------------------------------------
;; 7. state/sum
(define (state/sum x)
  (lambda (a)
    ((return-state a) (+ a x))
    ))

;;Testing
#|
((state/sum 5) 0)
((state/sum 2) 0)
((state/sum 2) 3)
(define traverse-state/sum
  (traverse return-state bind-state state/sum))
((traverse-state/sum '((1 . 2) . (3 . (4 . 5)))) 0);;((0 . 1) 3 6 . 10) 15
|#


;;---------------------------------------------------------
;; Master Testing

#|
(require "a12-student-tests.rkt")
(test-file #:file-name "a12.rkt")
|#