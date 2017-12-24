#lang racket

;;Won Yong Ha (woha)

;;Start: Feb 12 2016
;;End: Feb 15 2016

;;Assignment 4: Dynamic Scope

;;-------------------------------------------------------------------
;;Part 1

;;lex
(define lex
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           `(var ,(if (memv y env)
                      (- (length env) (length (memv y env)))
                      0))]
      [`,y #:when (number? y)
           `(const ,y)]
      [`(zero? ,n)
       `(zero? ,(lex n env))]
      [`(sub1 ,n)
       `(sub1 ,(lex n env))]
      [`(* ,n1 ,n2)
       `(* ,(lex n1 env)
           ,(lex n2 env))]
      [`(if ,condi ,whentrue ,whenfalse)
       `(if ,(lex condi env)
           ,(lex whentrue env)
           ,(lex whenfalse env))]
      [`(let ((,U ,n)) ,body)
       `(let ,(lex n env)
          ,(lex body env))]
      [`(lambda (,n) ,body)
       `(lambda ,(lex body (cons n env)))]
      [`(,rator ,rand)
       (cons (lex rator env)
             (cons (lex rand env) empty))]
      )))

;;Testing
#|\\(lex '((lambda (x) x) 5)  '())
(lex '(lambda (!)
        (lambda (n)
          (if (zero? n) 1 (* n (! (sub1 n))))))
     '())
(lex '(let ((! (lambda (!)
                 (lambda (n)
                   (if (zero? n) 1 (* n ((! !) (sub1 n))))))))
        ((! !) 5))
     '())|#

;;-------------------------------------------------------------------
;;Part 2

;;empty-env
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'fo-eulav "unbound variable ~s" y))))

;;extend-env
(define extend-env
  (lambda (x y env)
    `(extend-env ,x ,y ,env)))

;;apply-env
(define apply-env
  (lambda (env n)
    (match env
      (`(empty-env) `empty-env)
      (`(extend-env ,x ,y ,env) (if (eqv? x n)
                                    y
                                    (apply-env env n)))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;value-of-fn
(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (apply-env env y)]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(zero? ,exp)
       (= (value-of-fn exp env) 0)]
      [`(sub1 ,exp)
       (- (value-of-fn exp env) 1)]
      [`(* ,exp1 ,exp2)
       (* (value-of-fn exp1 env)
          (value-of-fn exp2 env))]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (value-of-fn condi env)
           (value-of-fn whentrue env)
           (value-of-fn whenfalse env))]
      [`(let ((,x ,y)) ,body)
       (let ([z (value-of-fn y env)])
         (value-of-fn body (extend-env x z env)))]
      [`(lambda (,y) ,body)
       (closure-fn y body env)]
      [`(,rator ,rand)
       (apply-closure-fn (value-of-fn rator env)
                         (value-of-fn rand env))]
      )))

;;apply-env-fn
#|(define apply-env-fn
  (lambda (env y)
    (env y)))|#

;;extend-env-fn
#|(define extend-env-fn
  (lambda (x exp env)
    (lambda (y)
      (if (eqv? x y)
          exp
          (apply-env-fn env y)))))|#

;;empty-env-fn
#|(define empty-env-fn
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound variable ~s" y))))|#

;;apply-closure-fn
(define apply-closure-fn
  (lambda (n1 n2)
    (n1 n2)))

;;closure-fn
(define closure-fn
  (lambda (y body env)
    (lambda (x)
      (value-of-fn body (extend-env y x env))
      )))

;;Testing
#|(value-of-fn 
 '((lambda (x) (if (zero? x) 
                   12 
                   47)) 
   0) 
 (empty-env))
(value-of-fn
 '(let ([y (* 3 4)])
    ((lambda (x) (* x y)) (sub1 6)))
 (empty-env))
(value-of-fn
 '(let ([x (* 2 3)])
    (let ([y (sub1 x)])
      (* x y)))
 (empty-env))
(value-of-fn
 '(let ([x (* 2 3)])
    (let ([x (sub1 x)])
      (* x x)))
 (empty-env))|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;value-of-ds
(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (apply-env env y)]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(zero? ,exp)
       (= (value-of-ds exp env) 0)]
      [`(sub1 ,exp)
       (- (value-of-ds exp env) 1)]
      [`(* ,exp1 ,exp2)
       (* (value-of-ds exp1 env)
          (value-of-ds exp2 env))]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (value-of-ds condi env)
           (value-of-ds whentrue env)
           (value-of-ds whenfalse env))]
      [`(let ((,y ,x)) ,body)
       (let ([z (value-of-ds x env)])
         (value-of-ds body (extend-env y z env)))]
      [`(lambda (,y) ,body)
       (closure-ds y body env)]
      [`(,rator ,rand)
       (apply-closure-ds (value-of-ds rator env)
                         (value-of-ds rand env))]
      )))

;;apply-env-ds
#|(define apply-env-ds
  (lambda (env y)
    (match env
      [`(empty-env)
       'error]
      [`(extend-env-ds ,x ,i ,env)
       (if (eqv? x y)
           i
           (apply-env-ds env y))]
      )))|#

;;extend-env-ds
#|(define extend-env-ds
  (lambda (x i env)
    `(extend-env-ds ,x ,i ,env)))|#

;;empty-env-ds
#|(define empty-env-ds
  (lambda ()
    (error 'value-of "unbound variable ~s" y)))|#

;;apply-closure-ds
(define apply-closure-ds
  (lambda (c a)
    (match c
      (`(closure ,x ,body ,env)
       (value-of-ds body (extend-env x a env)))
      )))

;;closure-ds
(define closure-ds
  (lambda (x body env)
    `(closure ,x ,body ,env)))

;;Testing
#|(value-of-ds
 '((lambda (x) (if (zero? x) 
                   12 
                   47)) 
   0) 
 (empty-env))
(value-of-ds
 '(let ([y (* 3 4)])
    ((lambda (x) (* x y)) (sub1 6)))
 (empty-env))
(value-of-ds
 '(let ([x (* 2 3)])
    (let ([y (sub1 x)])
      (* x y)))
 (empty-env))
(value-of-ds
 '(let ([x (* 2 3)])
    (let ([x (sub1 x)])
      (* x x)))
 (empty-env))|#

;;-------------------------------------------------------------------
;;Part 3

;;value-of-dynamic
(define value-of-dynamic
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (apply-env env y)]
      [`,y #:when (number? y)
           y]
      [`,y #:when(boolean? y)
           y]
      [`(quote ,exp)
       exp]
      [`(null? ,exp)
       (null? (value-of-dynamic exp env))]
      [`(zero? ,exp)
       (= (value-of-dynamic exp env) 0)]
      [`(sub1 ,exp)
       (- (value-of-dynamic exp env) 1)]
      [`(car ,exp)
       (car (value-of-dynamic exp env))]
      [`(cdr ,exp)
       (cdr (value-of-dynamic exp env))]
      [`(cons ,exp1 ,exp2)
       (cons (value-of-dynamic exp1 env)
             (value-of-dynamic exp2 env))]
      [`(* ,exp1 ,exp2)
       (* (value-of-dynamic exp1 env)
          (value-of-dynamic exp2 env))]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (value-of-dynamic condi env)
           (value-of-dynamic whentrue env)
           (value-of-dynamic whenfalse env))]
      [`(let ((,x ,y)) ,body)
       (let ([z (value-of-dynamic y env)])
         (value-of-dynamic body (extend-env x z env)))]
      [`(lambda (,y) ,body)
       `(lambda (,y) ,body)]
      [`(,rator ,rand)
       (match-let ((`(lambda (,y) ,body)
                    (value-of-dynamic rator env))
                   (`,x (value-of-dynamic rand env)))
         (value-of-dynamic body (extend-env y x env)))]
      )))

;;Testing
#|(value-of-dynamic '(let ([x 2])
                       (let ([f (lambda (e) x)])
                         (let ([x 5])
                           (f 0))))
                    (empty-env))
(value-of-dynamic
    '(let ([! (lambda (n)
                (if (zero? n) 
                    1
                    (* n (! (sub1 n)))))])
       (! 5))
    (empty-env))
(value-of-dynamic
    '(let ([f (lambda (x) (cons x l))])
       (let ([cmap 
	      (lambda (f)
		(lambda (l)               
		  (if (null? l) 
		      '()
		      (cons (f (car l)) ((cmap f) (cdr l))))))])
	 ((cmap f) (cons 1 (cons 2 (cons 3 '())))))) 
    (empty-env))|#