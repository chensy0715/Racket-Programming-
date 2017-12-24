#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Won Yong Ha
;;
;; CSCI-C 311
;; A8
;;
;; Start: 10 March 2016
;; End: 12 March 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Global Variable
(define m* 0)
(define n* 0)
(define v* 0)
(define k* 0)
(define ls* 0)

;;Pre-defined

;;empty-k
(define empty-k
  (lambda ()
    (lambda (v) v)))

;;empty-k-reg
(define empty-k-reg
  (lambda ()
    `(empty-k-reg)))

;;------------------------------------------------------------------------------
;; 1. ack-reg-driver

;;ack -> ack-cps
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (v) (ack-cps (sub1 m) v k)))])))

;;Testing
;;(ack-cps 2 2 (empty-k))

;;ack-reg----------------------------------------
(define ack-reg-driver
  (lambda (m n)
    (begin
     (set! m* m)
     (set! n* n)
     (set! k* (empty-k-reg))
     (ack-reg))))

;;else-k-reg
(define else-k-reg
  (lambda (m k)
    `(else-k-reg ,m ,k)))

(define apply-k-ack
  (lambda ()
    (match k*
      [`(else-k-reg ,m ,k)
       (begin
         (set! m* (sub1 m));;violation - fixed
         (set! k* k)
         (set! n* v*)
         (ack-reg))]
      [`(empty-k-reg) v*]
      ;;---------exception--------
      ;;apply-k directly
      [else
       (k* v*)]
      )))

(define ack-reg
  (lambda ()
    (cond
      [(zero? m*) (begin;;------------------
                    (set! k* k*)
                    (set! v* (add1 n*))
                    (apply-k-ack))]
      [(zero? n*) (begin
                    (set! m* (sub1 m*))
                    (set! n* 1)
                    (ack-reg))]
      [else (begin
              (set! n* (sub1 n*))
              (set! k* (else-k-reg m* k*))
              (ack-reg))])))

;;Testing
;;(ack-reg-driver 2 2)

;;------------------------------------------------------------------------------
;; 2. depth-reg

;;depth -> depth-cps
(define depth-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(pair? (car ls))
       (depth-cps (car ls)
	      (lambda (l)
		(depth-cps (cdr ls)
		       (lambda (r)
			 (let ((l (add1 l)))
			   (if (< l r) (k r) (k l)))))))]
      [else (depth-cps (cdr ls) k)])))

;;Testing
;;(depth-cps '(1 (2 (3 (4)))) (empty-k))

;;depth-reg----------------------------------------

;;Driver
(define depth-reg-driver
  (lambda (ls)
    (begin
      (set! ls* ls)
      (set! k* (empty-k-reg))
      (depth-reg))))

#| Already defined
(define empty-k-reg
  (lambda ()
    `(empty-k-reg)))
|#

;;depth-pair-1-k-reg
(define depth-pair-1-k-reg
  (lambda (ls k)
    `(pair-1 ,ls ,k)))

;;depth-pair-2-k-reg
(define depth-pair-2-k-reg
  (lambda (l k)
    `(pair-2 ,l ,k)))

;;apply-k-depth-reg
(define apply-k-depth-reg
  (lambda ()
    (match k*
      [`(empty-k-reg)
       v*]
      ;;First pair lambda-----------------------
      [`(pair-1 ,ls ,k)
       (begin;;--------------------
         (set! ls* (cdr ls))
         (set! k* (depth-pair-2-k-reg v* k))
         (depth-reg))]
      ;;Second pair lanbda---------------------
      [`(pair-2 ,l ,k)
       (begin
         (set! l (add1 l))
         ;;if-------------------if-start----------------
         (if (< l v*)
             ;;True
             (begin
               (set! k* k)
               (set! v* v*);;n/a
               (apply-k-depth-reg))
             ;;False
             (begin;;--------------------
               (set! k* k)
               (set! v* l)
               (apply-k-depth-reg))))];;-----------------------------------
      ;;if-------------------if-end----------------
      ;;---------exception--------
      ;;apply-k directly
      [else
       (k* v*)]
      )))

;;depth-reg
(define depth-reg
  (lambda ()
    (cond
      [(null? ls*) (begin;;---------------------
                     (set! k* k*)
                     (set! v* 1)
                     (apply-k-depth-reg))]
      [(pair? (car ls*)) (begin;;----------------
                           (set! ls* (car ls*))
                           (set! k* (depth-pair-1-k-reg ls* k*))
                           (depth-reg))];;pair
      [else (begin;;----------------------
              (set! ls* (cdr ls*))
              (set! k* k*)
              (depth-reg))];;else
      )))

;;Testing
;;(depth-reg-driver '(1 (2 (3 (4)))))

;;------------------------------------------------------------------------------
;; 3. fact-reg

;;fact -> fact-cps
(define fact-cps
  (lambda (n k)
    ((lambda (fact-cps k)
       (fact-cps fact-cps n k))
     (lambda (fact-cps n k)
       (cond
         [(zero? n) (k 1)]
         [else (fact-cps fact-cps (sub1 n) (lambda (v) (k (* n v))))]))
     k)))

;;Testing
;;(fact-cps 5 (empty-k))

;;fact-reg----------------------------------------

;;driver
(define fact-reg-driver
  (lambda (n)
    (begin
      (set! n* n)
      (set! k* (empty-k-reg))
      (fact-reg))))

#| Already defined
(define empty-k-reg
  (lambda ()
    `(empty-k-reg)))
|#

;;else-k-fact-reg
(define else-k-fact-reg
  (lambda (n k)
    `(else-k-fact-reg ,n ,k)))

;;apply-k-fact
(define apply-k-fact
  (lambda ()
    (match k*
      [`(empty-k-reg)
       v*];;----------------------
      [`(else-k-fact-reg ,n ,k)
       (begin;;---------
         (set! v* (* v* n))
         (set! k* k)
         (apply-k-fact))]
      ;;---------exception--------
      ;;apply-k directly
      [else (k* v*)]
      )))

;;fact-reg
(define fact-reg
  (lambda ()
    ;;--------------------------------
    ((lambda (fact-cps-ds);;first lambda--------------------
       (fact-cps-ds fact-cps-ds))
     (lambda (fact-cps);;second lambda----------------------
       (cond
         [(zero? n*)
          (begin;;---------------------
            (set! v* 1)
            (apply-k-fact))]
         [else
          (begin;;-------------------
            (set! k* (else-k-fact-reg n* k*))
            (set! n* (sub1 n*))
            (fact-reg))]
         )))
    ))

;;Testing
;;(fact-reg-driver 5)

;;------------------------------------------------------------------------------
;; 4. pascal-reg
(define pascal-cps
  (lambda (n k)
    (let ((pascal
           (lambda (pascal-cps k)
             (k (lambda (m a k)
		  (cond
		    [(> m n) (k '())]
		    [else (let ((a (+ a m)))
			    (pascal-cps pascal-cps (lambda (f) (f (add1 m) a (lambda (v) (k (cons a v)))))))]))))))
      (pascal pascal (lambda (f) (f 1 0 k))))))

;;Testing
;;(pascal-cps 10 (empty-k))

;;pascal-reg----------------------------------------

;;driver
(define pascal-reg-driver
  (lambda (n)
    (begin
      (set! n* 0)
      (set! k* (empty-k-reg))
      (pascal-reg))))

#| Already defined
(define empty-k-reg
  (lambda ()
    `(empty-k-reg)))
|#

;;else-k-pascal
(define else-k-pascal
  (lambda (x k)
    `(else-k-pascal ,x ,k)))

;;apply-k-pascal
(define apply-k-pascal
  (lambda ()
    (match k*
      [`(empty-k-reg)
       v*]
      [`(else-k-pascal ,x ,k)
       (begin (set! v* (cons x v*))
              (set! k* k)
              )]
      ;;---------exception--------
      ;;apply-k directly
      [else
       (k* v*)]
      )))

;;pascal-reg
(define pascal-reg
  (lambda ()
    (let ((pascal
           (lambda (pascal-cps k) 
              (k* (lambda (m* a* k*)
		  (cond
		    [(> m* n*) (begin
                                 (set! v* (apply-k-pascal k* '()))
                                 (pascal-reg))]
		    [else (begin
                           (set! a* (+ a* m*))
                           (set! k* (else-k-pascal a* k*))
                           (set! m* (+ 1 m*))
                           (pascal-reg))]
                    )))
             )))
      (pascal pascal (lambda (f) (f 1 0 k*)))
      )))

;;Testing
;;(pascal-reg-driver 10)

;;------------------------------------------------------------------------------
;;Master Testing
;;(require "a8-student-tests.rkt")
;;(test-file #:file-name "a8.rkt")