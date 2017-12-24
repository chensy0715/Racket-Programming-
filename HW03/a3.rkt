#lang racket
;;HW3
;;Won Yong Ha (woha)


;;value-of

(define value-of
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (env y)]
      [`,y #:when (number? y)
           y]
      [`,y #:when(boolean? y)
           y]
      [`(zero? ,exp)
       (= (value-of exp env) 0)]
      [`(sub1 ,exp)
       (- (value-of exp env) 1)]
      [`(* ,exp1 ,exp2)
       (* (value-of exp1 env)
          (value-of exp2 env))]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (value-of condi env)
           (value-of whentrue env)
           (value-of whenfalse env))]
      [`(let ((,x ,y)) ,body)
       (let ([z (value-of y env)])
         (value-of body
                   (lambda (i)
                     (if (eqv? x i)
                         z
                         (env i)))))]
      [`(lambda (,x) ,body)
       (lambda (i)
         (value-of body
                   (lambda (y)
                     (if (eqv? x y)
                         i
                         (env y)))))]
      [`(,rator ,rand)
       ((value-of rator env)
        (value-of rand env))]
      )))

;;------------------------------------------------------------
;;testing
#|(value-of
   '((lambda (x) (if (zero? x)
                     #t
                     #f))
     0)
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
(value-of (test-file #:file-name "a3.rkt")
   '((lambda (x) (if (zero? x) 
                     12 
                     47)) 
     0) 
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
(value-of
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
(value-of
   '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
(value-of
   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
(value-of 
   '(let ((! (lambda (x) (* x x))))
      (let ((! (lambda (n)
                 (if (zero? n) 1 (* n (! (sub1 n)))))))
        (! 5)))
   (lambda (y) (error 'value-of "unbound variable ~s" y)))
(value-of
   '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5)
   (lambda (y) (error 'value-of "unbound variable ~s" y)))|#
;;------------------------------------------------------------

;;value-of-fn
(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (apply-env-fn env y)]
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
         (value-of-fn body (extend-env-fn x z env)))]
      [`(lambda (,y) ,body)
       (lambda (x)
         (value-of-fn body (extend-env-fn y x env)))]
      [`(,rator ,rand)
       ((value-of-fn rator env)
        (value-of-fn rand env))]
      )))

;;empty-env
(define empty-env-fn
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound variable ~s" y))))

;apply-env-fn
(define apply-env-fn
  (lambda (env y)
    (env y)))

;extend-env-fn
(define extend-env-fn
  (lambda (x exp env)
    (lambda (y)
      (if (eqv? x y)
          exp
          (apply-env-fn env y)))))

;;------------------------------------------------------------
;testing
#|(value-of-fn
   '((lambda (x) (if (zero? x)
                     #t
                     #f))
     0)
   (empty-env-fn))
(value-of-fn 
   '((lambda (x) (if (zero? x) 
                     12 
                     47)) 
     0) 
   (empty-env-fn))
(value-of-fn
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   (empty-env-fn))
(value-of-fn
   '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))
   (empty-env-fn))
(value-of-fn
   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   (empty-env-fn))
(value-of-fn
   '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5)
   (empty-env-fn))|#
;;------------------------------------------------------------

;;value-of-ds
(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (apply-env-ds env y)]
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
         (value-of-ds body (extend-env-ds y z env)))]
      [`(lambda (,y) ,body)
       (lambda (x)
         (value-of-ds body (extend-env-ds y x env)))]
      [`(,rator ,rand)
       ((value-of-ds rator env)
        (value-of-ds rand env))]
      )))

;;apply-env-ds
(define apply-env-ds
  (lambda (env y)
    (match env
      [`(empty-env)
       'error]
      [`(extend-env-ds ,x ,i ,env)
       (if (eqv? x y)
           i
           (apply-env-ds env y))]
      )))

;;extend-env-ds
(define extend-env-ds
  (lambda (x i env)
    `(extend-env-ds ,x ,i ,env)))

;;empty-env-ds
(define empty-env-ds
  (lambda ()
    `(empty-env)))

;;------------------------------------------------------------
;;testing
#|(value-of-ds
   '((lambda (x) (if (zero? x)
                     #t
                     #f))
     0)
   (empty-env-ds))
(value-of-ds
   '((lambda (x) (if (zero? x) 
                     12 
                     47)) 
     0) 
   (empty-env-ds))
(value-of-ds
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   (empty-env-ds))
(value-of-ds
   '(let ([x (* 2 3)])
      (let ([y(test-file #:file-name "a3.rkt") (sub1 x)])
        (* x y)))
   (empty-env-ds))
(value-of-ds
   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   (empty-env-ds))
(value-of-ds
   '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5)
   (empty-env-ds))|#
;;------------------------------------------------------------

;;fo-eulav
(define fo-eulav
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (env y)]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(,exp ?orez)
       (= (fo-eulav exp env) 0)]
      [`(,exp 1bus)
       (- (fo-eulav exp env) 1)]
      [`(,exp1 ,exp2 *)
       (* (fo-eulav exp2 env)
          (fo-eulav exp1 env))]
      [`(,whenfalse ,whentrue ,condi fi)
       (if (fo-eulav condi env)
           (fo-eulav whentrue env)
           (fo-eulav whenfalse env))]
      [`(,body ((,x ,y)) tel)
       (let ([a (fo-eulav x env)])
         (fo-eulav body
                   (lambda (i)
                     (if (eqv? y i)
                         a
                         (env i)))))]
      [`(,body (,y) adbmal)
       (lambda (i)
         (fo-eulav body
                   (lambda (x)
                     (if (eqv? y x)
                         i
                         (env x)))))]
      
      [`(,rand ,rator)
       ((fo-eulav rator env)
        (fo-eulav rand env))]
      )))

;;empty-env
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'fo-eulav "unbound variable ~s" y))))

;;------------------------------------------------------------
;;testing
#|(fo-eulav '(5 (x (x) adbmal)) (lambda (y) (error 'fo-eulav "unbound variable ~s" y)))
(fo-eulav '(((x 1bus) (x) adbmal) ((5 f) (f) adbmal)) (lambda (y) (error 'fo-eulav "unbound variable ~s" y)))
(fo-eulav   '(5
 (((((((n 1bus) (f f)) n *) 1 (n ?orez) fi)
                                   (n) adbmal)
                                     (f) adbmal)
  ((((((n 1bus) (f f)) n *) 1 (n ?orez) fi)
                                  (n) adbmal)
                                    (f) adbmal))) 
 (lambda (y) (error 'fo-eulav "unbound variable ~s" y)))|#
;;------------------------------------------------------------