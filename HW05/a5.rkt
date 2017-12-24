#lang racket

;;Won Yong Ha (woha)

;;Start: Feb 17 2016
;;End: Feb 20 2016

;;Assignment 5: Parameter-Passing Conventions

;;--------------------------------------------------------------------
;;Basic Definitions
;;From Assignment 4

;;empty-env
(define empty-env
  (lambda ()
    (lambda (y)
      (error "unbound variable ~s" y))))

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

;;apply-clouse
(define apply-closure
  (lambda (n1 n2)
    (n1 n2)))

;;make-clouse
(define make-closure
  (lambda (y body env)
    (lambda (x)
      (value-of body (extend-env y x env))
      )))

;;Testing Example
(define random-sieve
  '((lambda (n)
      (if (zero? n)
          (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
          (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
    (random 2)))

;;--------------------------------------------------------------------
;;value-of
;;Example Part
(define value-of
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (value-of n env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      ;;begin
      [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
      ;;random
      [`(random ,n) (random (value-of n env))]
      ;;rand -> symbol
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(lambda (,x) ,body) (make-closure x body env)]
      ;;normal rator, rand
      [`(,rator ,rand) (apply-closure (value-of rator env)
                                      (value-of rand env))]
      )))

;;Actual Question Part

;;--------------------------------------------------------------------
;;value-of-cbv
(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (unbox (apply-env env y))]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(zero? ,n)
       (= (val-of-cbr n env) 0)]
      [`(sub1 ,x)
       (- (val-of-cbr x env) 1)]
      [`(* ,x ,y)
       (* (val-of-cbr x env)
          (val-of-cbr y env))]
      [`(lambda (,x) ,body)
       (closure-cbr x body env)]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (val-of-cbr condi env)
           (val-of-cbr whentrue env)
           (val-of-cbr whenfalse env))]
      ;;random
      [`(random ,n)
       (random (val-of-cbr n env))]
      ;;begin
      [`(begin2 ,e1 ,e2)
       (begin (val-of-cbr e1 env)
              (val-of-cbr e2 env))]
      ;;set!
      [`(set! ,x ,rhs)
       (set-box! (apply-env env x)
                 (val-of-cbr rhs env))]
      ;;rand -> symbol
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbr rator env)
                                      (apply-env env rand))]
      ;;normal rator, rand
      [`(,rator ,rand)
       (apply-closure (val-of-cbr rator env)
                      (box (val-of-cbr rand env)))]
      )))

;;closure-cbr
(define closure-cbr
  (lambda (y body env)
    (lambda (c)
      (val-of-cbr body (extend-env y c env)))))

;;Testing
#|(val-of-cbr
   '((lambda (x) (begin2 (set! x #t)
                         (if x 3 5))) #f)
   (empty-env))
(val-of-cbr
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))
(val-of-cbr
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))
(val-of-cbr
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env))|#

;;--------------------------------------------------------------------
;;value-of-cbv
(define val-of-cbv
  (lambda (exp env) 
    (match exp
      [`,y #:when (symbol? y)
           (unbox (apply-env env y))]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(zero? ,n)
       (= (val-of-cbv n env) 0)]
      [`(sub1 ,x)
       (- (val-of-cbv x env) 1)]
      [`(* ,x ,y)
       (* (val-of-cbv x env)
          (val-of-cbv y env))]
      [`(lambda (,x) ,body)
       (closure-cbv x body env)]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (val-of-cbv condi env)
           (val-of-cbv whentrue env)
           (val-of-cbv whenfalse env))]
      ;;random
      [`(random ,n)
       (random (val-of-cbv n env))]
      ;;begin
      [`(begin2 ,e1 ,e2)
       (begin (val-of-cbv e1 env)
              (val-of-cbv e2 env))]
      ;;set!
      [`(set! ,x ,rhs)
       (set-box! (apply-env env x)
                 (val-of-cbv rhs env))]
      ;;rand -> symbol
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbv rator env)
                                      (box (apply-env env rand)))]
      ;;normal rator, rand
      [`(,rator ,rand)
       (apply-closure (val-of-cbv rator env)
                      (box (val-of-cbv rand env)))]
      )))

;;closure-cbv
(define closure-cbv
  (lambda (y body env)
    (lambda (c)
      (val-of-cbv body (extend-env y c env)))))

;;Testing
#|(val-of-cbv
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))
(val-of-cbv
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))
(val-of-cbv
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env))|#

;;--------------------------------------------------------------------
;;value-of-cbname
(define val-of-cbname
  (lambda (exp env)
    (match exp
      ;;Call-by-Name
      [`,y #:when (symbol? y)
           ((unbox (apply-env env y)))]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(zero? ,n)
       (= (val-of-cbname n env) 0)]
      [`(sub1 ,x)
       (- (val-of-cbname x env) 1)]
      [`(* ,x ,y)
       (* (val-of-cbname x env)
          (val-of-cbname y env))]
      [`(lambda (,x) ,body)
       (closure-cbname x body env)]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (val-of-cbname condi env)
           (val-of-cbname whentrue env)
           (val-of-cbname whenfalse env))]
      ;;random
      [`(random ,n)
       (random (val-of-cbname n env))]
      ;;begin
      [`(begin2 ,e1 ,e2)
       (begin (val-of-cbname e1 env)
              (val-of-cbname e2 env))]
      ;;set!
      [`(set! ,x ,rhs)
       (set-box! (val-of-cbname env x)
                 (val-of-cbname rhs env))]
      ;;rand -> symbol
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbname rator env)
                                      (apply-env env rand))]
      ;;normal rator, rand
      [`(,rator ,rand)
       (apply-closure (val-of-cbname rator env)
                      (box (lambda () (val-of-cbname rand env))))]
      )))

;;closure-cbname
(define closure-cbname
  (lambda (y body env)
    (lambda (c)
      (val-of-cbname body (extend-env y c env)))))

;;Testing
#|(val-of-cbname random-sieve (empty-env))|#

;;--------------------------------------------------------------------
;;val-of-cbneed
(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y)
           (let ([b (apply-env env y)])
             (let ([val ((unbox b))])
               (set-box! b (lambda () val))
               val))]
      [`,y #:when (number? y)
           y]
      [`,y #:when (boolean? y)
           y]
      [`(zero? ,n)
       (= (val-of-cbneed n env) 0)]
      [`(sub1 ,x)
       (- (val-of-cbneed x env) 1)]
      [`(* ,x ,y)
       (* (val-of-cbneed x env)
          (val-of-cbneed y env))]
      [`(lambda (,x) ,body)
       (closure-cbneed x body env)]
      [`(if ,condi ,whentrue ,whenfalse)
       (if (val-of-cbneed condi env)
           (val-of-cbneed whentrue env)
           (val-of-cbneed whenfalse env))]
      ;;random
      [`(random ,n)
       (random (val-of-cbneed n env))]
      ;;begin
      [`(begin2 ,e1 ,e2)
       (begin (val-of-cbneed e1 env)
              (val-of-cbneed e2 env))]
      ;;set!
      [`(set! ,x ,rhs)
       (set-box! (val-of-cbneed env x)
                 (val-of-cbneed rhs env))]
      ;;rand -> symbol
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbneed rator env)
                                      (apply-env env rand))]
      ;;normal rator, rand
      [`(,rator ,rand)
       (apply-closure (val-of-cbneed rator env)
                      (box (lambda () (val-of-cbneed rand env))))]
      )))

;;closure-cbneed
(define closure-cbneed
  (lambda (y body env)
    (lambda (c)
      (val-of-cbneed body (extend-env y c env)))))

;;Testing
#|(val-of-cbneed random-sieve (empty-env))|#

#|(require "a5-student-tests.rkt")
(test-file #:file-name "a5.rkt")|#