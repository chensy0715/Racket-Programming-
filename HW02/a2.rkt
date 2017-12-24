#lang racket

;;Assignment 2

;; Q01 list-ref

(define list-ref
  (lambda (ls n)
    (letrec ((fn
              (lambda (n)
                (if (zero? n)
                    ls
                    (cdr (fn (sub1 n))))
                )))
      (car (fn n))
      )))

;;(list-ref '(a b c) 2)
;;(list-ref '(a b c) 0)

;; Q02 union

(define union
  (lambda (ls1 ls2)
    (cond
      [(null? ls1)
       ls2]
      [(memv (car ls1) ls2)
       (union (cdr ls1) ls2)]
      [else
       (cons (car ls1) (union (cdr ls1) ls2))])
    ))

;;(union '() '())
;;(union '(x) '())
;;(union '(x) '(x))
;;(union '(x y) '(x z))

;; Q03 extend

(define extend
  (lambda (x pred)
    (lambda (num)
      (or (eqv? x num) (pred num))
      )))

;((extend 1 even?) 0)
;((extend 1 even?) 1)
;((extend 1 even?) 2)
;((extend 1 even?) 3)
;(filter (extend 1 even?) '(0 1 2 3 4 5))
;(filter (extend 3 (extend 1 even?)) '(0 1 2 3 4 5))
;(filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 4 5))

;; Q04 walk-symbol

(define walk-symbol
  (lambda (ch ls)
    (let ([bd (assv ch ls)])
      (cond
        [(not bd)
         ch]
        [(symbol? (cdr bd))
         (walk-symbol (cdr bd) ls)]
        [else
         (cdr bd)])
      )))

;;(walk-symbol 'a '((a . 5)))
;;(walk-symbol 'a '((b . c) (a . b)))
;;(walk-symbol 'a '((a . 5) (b . 6) (c . a)))
;;(walk-symbol 'c '((a . 5) (b . (a . c)) (c . a)))
;;(walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))
;;(walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)))
;;(walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e)))

;; Q05 lambda->lumbda

(define lambda->lumbda
  (lambda (exp)
    (match exp
      [`(lambda (,m) ,y)
       `(lumbda (,m) ,(lambda->lumbda y))]
      [`(,rator ,rand)
       `(,(lambda->lumbda rator)
         ,(lambda->lumbda rand))]
      [`,m (not (pair? m))
           m])
    ))

;;(lambda->lumbda 'x)
;;(lambda->lumbda '(lambda (x) x))
;;(lambda->lumbda '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
;;(lambda->lumbda '(lambda (lambda) lambda)) 
;;(lambda->lumbda '((lambda (lambda) lambda) (lambda (y) y)))
;;(lambda->lumbda '((lambda (x) x) (lambda (x) x)))

;; Q06 var-occurs?

(define var-occurs?
  (lambda (varnm exp)
    (match exp
      [`(lambda (,m) ,y)
       (var-occurs? varnm y)]
      [`(,rator ,rand)
       (or (var-occurs? varnm rator)
           (var-occurs? varnm rand))]
      [`,m (not (pair? m))
           (eq? varnm m)])
    ))

;;(var-occurs? 'x 'x) 
;;(var-occurs? 'x '(lambda (x) y))
;;(var-occurs? 'x '(lambda (y) x))
;;(var-occurs? 'x '((z y) x))

;; Q07 vars

(define vars
  (lambda (exp)
    (match exp
      [`(lambda (,m) ,y)
       (vars y)]
      [`(,rator ,rand)
       (append (vars rator)
               (vars rand))]
      [`,m (not (pair? m))
       `(,m)])
    ))

;;(vars 'x)
;;(vars '(lambda (x) x))
;;(vars '((lambda (y) (x x)) (x y)))
;;(vars '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))

;; Q08 unique-vars

(define unique-vars
  (lambda (exp)
    (match exp
      [`(lambda (,m) ,y)
       (unique-vars y)]
      [`(,rator ,rand)
       (union (unique-vars rator)
              (unique-vars rand))]
      [`,m (not (pair? m))
       `(,m)])
    ))

;;(unique-vars '((lambda (y) (x x)) (x y)))
;;(unique-vars '((lambda (z) (lambda (y) (z y))) x))
;;(unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a))))

;; Q09 var-occurs-free?

(define var-occurs-free?
  (lambda (sym exp)
    (match exp
      [`(lambda (,m) ,y)
       (if (eq? m sym)
           #f
           (var-occurs-free? sym y))]
      [`(,rator ,rand)
       (or (var-occurs-free? sym rator)
           (var-occurs-free? sym rand))]
      [`,m (not (pair? m))
           (eq? m sym)])
    ))

;;(var-occurs-free? 'x 'x)
;;(var-occurs-free? 'x '(lambda (y) y))
;;(var-occurs-free? 'x '(lambda (x) (x y)))
;;(var-occurs-free? 'x '(lambda (x) (lambda (x) x))) 
;;(var-occurs-free? 'y '(lambda (x) (x y)))
;;(var-occurs-free? 'y '((lambda (y) (x y)) (lambda (x) (x y))))
;;(var-occurs-free? 'x '((lambda (x) (x x)) (x x)))

;; Q10 var-occurs-bound?

(define var-occurs-bound?
  (lambda (sym exp)
    (match exp
      [`(,rator ,rand)
       (or (var-occurs-bound? sym rator)
           (var-occurs-bound? sym rand))]
      [`(lambda (,m) ,y)
       (cond
         [(memv sym (vars y))
          (cond
            ((eq? sym m) #t)
            (else (var-occurs-bound? sym y)))]
         (else #f))]
      [`,m (symbol? m)
           #f])
    ))

;;(var-occurs-bound? 'x 'x)
;;(var-occurs-bound? 'x '(lambda (x) x))
;;(var-occurs-bound? 'y '(lambda (x) x))
;;(var-occurs-bound? 'x '((lambda (x) (x x)) (x x)))
;;(var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z))))
;;(var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z))))
;;(var-occurs-bound? 'x '(lambda (x) y))
;;(var-occurs-bound? 'x '(lambda (x) (lambda (x) x)))

;; Q11 unique-free-vars

(define unique-free-vars
  (lambda (exp)
    (match exp
      [`(,rator ,rand)
       (union (unique-free-vars rator)
              (unique-free-vars rand))]
      [`(lambda (,m) ,y)
       (remv m (unique-free-vars y))]
      [`,m (symbol? m)
       `(,m)])
    ))

;;(unique-free-vars 'x)
;;(unique-free-vars '(lambda (x) (x y)))
;;(unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))

;; Q12 unique-bound-vars

(define unique-bound-vars
  (lambda (exp)
    (match exp
      [`(,rator ,rand)
       (union (unique-bound-vars rator)
              (unique-bound-vars rand))]
      [`(lambda (,m) ,y)
       (if (memv m (unique-vars y))
           (cons m (unique-bound-vars y))
           (unique-bound-vars y))]
      [`,m (symbol? m)
           '()])
    ))

;;(unique-bound-vars 'x)
;;(unique-bound-vars '(lambda (x) y))
;;(unique-bound-vars '(lambda (x) (x y)))
;;(unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
;;(unique-bound-vars '(lambda (y) y))
;;(unique-bound-vars '(lambda (x) (y z)))

;; Q13 lex

(define lex
  (lambda (exp accum)
    (match exp
      [`(,rator ,rand)
       (list (lex rator accum)
             (lex rand accum))]
      [`(lambda (,m) ,body)
       `(lambda ,(lex body (cons m accum)))]
      [`,m (symbol? m)
           `(var ,(- (length accum)
                     (length (memv m accum))))
               ])
    ))

(lex '(lambda (x) x) '())
(lex '(lambda (y) (lambda (x) y)) '())
(lex '(lambda (y) (lambda (x) (x y))) '())
(lex '(lambda (x) (lambda (x) (x x))) '())
(lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
(lex '(lambda (a)
          (lambda (b)
            (lambda (c)
              (lambda (a)
                (lambda (b)
                  (lambda (d)
                    (lambda (a)
                      (lambda (e)
                        (((((a b) c) d) e) a))))))))) '())
(lex '(lambda (a)
          (lambda (b)
	    (lambda (c)
	      (lambda (w)
	        (lambda (x)
		  (lambda (y)
		    ((lambda (a)
		       (lambda (b)
			 (lambda (c)
			   (((((a b) c) w) x) y))))
		     (lambda (w)
		       (lambda (x)
			 (lambda (y)
			   (((((a b) c) w) x) y))))))))))) '())