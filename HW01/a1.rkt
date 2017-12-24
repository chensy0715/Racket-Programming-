#lang racket

;;Won Yong Ha (woha)
;;Assignment 1
;; 20 Jan 2016

;;#1 countdown
;; Int -> List
(define countdown
  (lambda (n)
    (cond
      [(zero? n) '(0)]
      [else (cons n (countdown (- n 1)))]
      )))

;;(countdown 5)

;;#2 insertR
;; Symbol Symbol List -> List
(define insertR
  (lambda (x r ls)
    (cond
      [(empty? ls) ls]
      [(eqv? x (car ls)) (cons x (cons r (insertR x r (cdr ls))))]
      [else (cons (car ls) (insertR x r (cdr ls)))]
      )))

;;(insertR 'x 'y '(x z z x y x))

;;#3 remv-1st
;; Symbol List -> List
(define remv-1st
  (lambda (s ls)
    (cond
      [(empty? ls) ls]
      [(eqv? (car ls) s) (cdr ls)]
      [else (cons (car ls) (remv-1st s (cdr ls)))]
      )))

;(remv-1st 'x '(x y z x))
;(remv-1st 'y '(x y z y x))

;;#4 list-index-ofv?
;; Symbol List -> List
(define list-index-ofv?
  (lambda (x ls)
    (cond
      [(empty? ls) 1]
      [(eq? x (car ls)) 0]
      [else (+ 1 (list-index-ofv? x (cdr ls)))]
      )))

;;(list-index-ofv? 'x '(x y z x x))
;;(list-index-ofv? 'x '(y z x x))

;;#5 filter-fr
;; Predication List -> List
(define filter-fr
  (lambda (pre ls)
    (cond
      [(empty? ls) ls]
      [(pre (car ls)) (cons (car ls) (filter-fr pre (cdr ls)))]
      [else (filter-fr pre (cdr ls))]
      )))

;;(filter-fr even? '(1 2 3 4 5 6))

;;#6 zip
;; List List -> List
(define zip
  (lambda (ls1 ls2)
    (cond
      [(or (empty? ls1) (empty? ls2)) empty]
      [else (cons (list (car ls1) (car ls2)) (zip (cdr ls1) (cdr ls2)))]
      )))

;;(zip '(1 2 3) '(a b c))

;;#7 map-fr
;; Precondition List -> List
(define map-fr
  (lambda (pre ls)
    (cond
      [(empty? ls) ls]
      [else (cons (pre (car ls)) (map pre (cdr ls)))]
      )))

;;(map-fr add1 '(1 2 3 4))

;;#8 append-fr
;; List -> List
(define append-fr
  (lambda (ls1 ls2)
    (cond
      [(empty? ls1) ls2]
      [else (cons (car ls1) (append-fr (cdr ls1) ls2))]
      )))

;;(append-fr '(a b c) '(1 2 3))

;;#9 reverse-fr
;; List -> List
(define reverse-fr
  (lambda (ls)
    (if (null? ls)
        '()
        (append (reverse-fr (cdr ls)) (list (car ls)))
        )))

;;(reverse-fr '(a 3 x))

;;#10 fact
;; Int -> Int
(define fact
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (fact (- n 1)))]
      )))

;;(fact 0)
;;(fact 5)

;;#11 memv-fr
;; Char List -> List | False
(define memv-fr
  (lambda (ch ls)
    (cond
      [(empty? ls) false]
      [(eqv? ch (car ls)) ls]
      [else (memv-fr ch (cdr ls))]
      )))

;;(memv-fr 'a '(a b c))
;;(memv-fr 'b '(a ? c))
;;(memv-fr 'b '(a b c d))

;;#12 fib
;; Int -> Int
(define fib
  (lambda (n)
    (if (<= n 2)
        (if (= n 0)
            0
            1
            )
        (+ (fib (- n 2)) (fib (- n 1)))
        )))

;;(fib 0)
;;(fib 1)
;;(fib 7)

;;#13
;;(equal? '((w x) y (z)) '((w . (x . ())) . (y . ((z . ()) . ()))))

;;#14 binary-natural
;; List -> Int
(define binary-natural
  (lambda (ls)
    (cond
      [(empty? ls) 0]
      [else (+ (car ls) (* 2 (binary-natural (cdr ls))))]
      )))

;;(binary-natural '())
;;(binary-natural '(0 0 1))
;;(binary-natural '(0 0 1 1))
;;(binary-natural '(1 1 1 1))
;;(binary-natural '(1 0 1 0 1))
;;(binary-natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))

;;#15 minus
;; Int Int -> Int
(define minus
  (lambda (a b)
    (cond
      [(zero? b) a]
      [else (+ -1 (minus a (- b 1)))]
      )))

;;(minus 5 3)
;;(minus 100 50)

;;#16 div
;; Int Int -> Int
(define div
  (lambda (a b)
    (cond
      [(zero? a) 0]
      [else (+ 1 (div (- a b) b))]
      )))

;;(div 25 5)
;;(div 36 6)

;;#17 append-map-fr
;; Precondition List -> List
(define append-map-fr
  (lambda (pre ls)
    (cond
      [(empty? ls) ls]
      [else (append (pre (car ls)) (append-map-fr pre (cdr ls)))]
      )))

;;(append-map-fr countdown (countdown 5))

;;#18 set-difference
;; List List -> List
(define set-difference
  (lambda (ls1 ls2)
    (cond
      [(empty? ls1) ls1]
      [(not (member (car ls1) ls2)) (cons (car ls1) (set-difference (cdr ls1) ls2))]
      [else (set-difference (cdr ls1) ls2)]
      )))

;;(set-difference '(1 2 3 4 5) '(2 4 6 8))