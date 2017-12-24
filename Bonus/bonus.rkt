#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Won Yong Ha
;;
;; CSCI-C 311
;; Assignment Bonus
;;
;; Start: 4 April 2016
;; End: 6 April 2016
;;
;; Comments:
;; There is two 5 in this assignment so total 8 questions!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;-----------------------------------------------------
;; Part 1


;;--------------------------------------
;; Question 1

#|
;;filter
(define filter
  (lambda (pre ls)
    (cond
      [(empty? ls) ls]
      [(pre (car ls)) (cons (car ls) (filter pre (cdr ls)))]
      [else (filter pre (cdr ls))]
      )))
|#

;;filter-sps
(define filter-sps
  (lambda (pre ls s)
    (cond
      [(empty? ls) (values ls s)]
      [(pre (car ls)) (let-values (((v ls2) (filter-sps pre (cdr ls) s)))
                        (values (cons (car ls) v) ls2))]
      [else (let-values (((v ls2) (filter-sps pre (cdr ls) s)))
              (values v (cons (car ls) ls2)))]
      )))

;;Testing
#|
(filter-sps even? '(1 2 3 4 5 6 7 8 9 10) '())
(filter-sps odd? '(1 2 3 4 5 6 7) '())
(filter-sps (lambda (x) (or (> x 6) (< x 2))) '(1 2 3 4 5 6 7) '())
|#

;;--------------------------------------
;; Question 2

#|
;;filter*
(define filter*
  (lambda (f ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cons (filter* f (car ls)) (filter* f (cdr ls)))]
      [(null? (car ls)) '()]
      [(f (car ls)) (cons (car ls) (filter* f (cdr ls)))]
      [else (filter* f (cdr ls))]
      )))

;;Testing
(filter* even? '(1 2 3 4 5 6))
(filter* odd? '(1 (2 3 (4 5)) 6 7))
(filter* (lambda (x) (or (even? x) (< 7 x))) '(1 (2 3 (4 5)) 6 7 ((8 9) 10)))
|#

;;filter*-sps
(define filter*-sps
  (lambda (f ls a)
    (cond
      [(null? ls) (values ls a)]
      [(pair? (car ls))
       (let-values (((v1 a2) (filter*-sps f (cdr ls) a)))
         (let-values (((v2 a3) (filter*-sps f (car ls) a)))
           (values (cons v2 v1) (cons a3 a2))))]
      [(null? (car ls)) (values ls a)]
      [(f (car ls)) (let-values (((v ls2) (filter*-sps f (cdr ls) a)))
                      (values (cons (car ls) v) ls2))]
      [else (let-values (((v ls2) (filter*-sps f (cdr ls) a)))
              (values v (cons (car ls) ls2)))]
      )))

;;Testing
#|
(filter*-sps even? '(1 2 3 4 5 6) '())
(filter*-sps odd? '(1 (2 3 (4 5)) 6 7) '())
(filter*-sps (lambda (x) (or (even? x) (< 7 x))) '(1 (2 3 (4 5)) 6 7 ((8 9) 10)) '())
|#

;;--------------------------------------
;; Question 3

#|
;;fib
(define fib
  (lambda (n)
    (cond
      [(zero? n) 0]
      [(eqv? n 1) 1]
      [else (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))])))
|#

;;fib-sps - provided from lecture
(define fib-sps
  (lambda (n s)
    (cond
      [(assv n s) => (lambda (pr) (values (cdr pr) s))]
      [(< n 2) (values n (cons (cons n n) s))]
      [else
       (let-values (((v s2) (fib-sps (- n 2) s)))
         (let-values (((v2 s3) (fib-sps (- n 1) s2)))
           (values (+ v v2) (cons (cons n (+ v v2)) s3))))]
      )))

;;Testing
#|
(fib-sps 0 '())
(fib-sps 1 '())
(fib-sps 3 '())
(fib-sps 10 '())
|#

;;-----------------------------------------------------
;; Part 2

;;--------------------------------------
;; Question 4

;;and*
(define-syntax and*
  (syntax-rules ()
    [(and*) #t]
    [(and* n) n]
    [(and* start ... end) end]
    ))

;;Testing
#|
(and* 1 2 3)
(and* #f)
(and*)
(and* 'a)
|#

;;--------------------------------------
;; Question 5

;;list*
(define-syntax list*
  (syntax-rules ()
    [(list* ,n) ,n]
    [(list* start end ...) (cons start (list* end ...))]
    ))

;;Testing
#|
(list* 'a 'b 'c 'd)
(list* 'a)
|#

;;--------------------------------------
;; Question 6

;;macro-list
(define-syntax macro-list
  (syntax-rules ()
    [(macro-list) '()]
    [(macro-list start end ...) (cons start (macro-list end ...))]
    ))

;;Testing
#|
(define list (lambda a a))
(list 1 2 3 4)
(macro-list);;()
(macro-list 1 'b 2 'd);;(1 b 2 d)
|#

;;--------------------------------------
;; Question 7

;;mcond
(define-syntax mcond
  (syntax-rules ()
    [(mcond (else ,n)) ,n]
    [(mcond (#f n)) #f]
    [(mcond (#t n)) n]
    [(mcond n) n]
    [(mcond e1 e2 ...) (if (mcond e1) (mcond e1) (mcond e2 ...))]
    ))

#|
;;Testing
(mcond
 (#f #t)
 (else 'dog))
(mcond 
 (else 'cat))
;;(mcond 
;; (#t #t) 
;; (unbound variables))
|#

;;--------------------------------------
;; Question 8

;;Genenal Test
#|
(map (lambda (x) (list x x)) '(a b c))
(define-syntax copy-code
  (syntax-rules ()
    [(_ x) `(,x x)]))
(copy-code 'a)
(map copy-code '(a b c))
|#

;;Marcro-map
(define-syntax macro-map
  (syntax-rules ()
    [(macro-map ,pre (start end ...)) (cons (pre start) (macro-map pre (end ...)))]
    [(macro-map pre end) end]
    ))

;;Testing
#|
(macro-map quote '((trinidad and tobago) (saint vincent and the grenadines) (antigua and barbuda)))
(macro-map copy-code '((lambda (x) x) (lambda (x) (+ 2 x)) (lambda (x) 7)))
(define-syntax quote-quote
  (syntax-rules ()
    [(_ e) (quote (quote e))]))
(macro-map quote-quote '((trinidad and tobago) (saint vincent and the grenadines) (antigua and barbuda)))
|#