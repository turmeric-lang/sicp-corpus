;; SICP 1.1 -- The Elements of Programming.
;; Code and stated results from Structure and Interpretation of Computer
;; Programs, 2nd ed., by Harold Abelson and Gerald Jay Sussman with Julie
;; Sussman (MIT Press), CC BY-SA 4.0. See ../NOTICE.
(import (scheme base) (scheme write) (srfi 216))

(define (show x) (display x) (newline))

;; 1.1.1 Expressions
(show 486)
(show (+ 137 349))
(show (- 1000 334))
(show (* 5 99))
(show (/ 10 5))
(show (+ 2.7 10))
(show (+ 21 35 12 7))
(show (* 25 4 12))
(show (+ (* 3 5) (- 10 6)))
(show (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6)))

;; 1.1.2 Naming and the Environment
(define size 2)
(show size)
(show (* 5 size))
(define pi 3.14159)
(define radius 10)
(show (* pi (* radius radius)))
(define circumference (* 2 pi radius))
(show circumference)

;; 1.1.4 Compound Procedures
(define (square x) (* x x))
(show (square 21))
(show (square (+ 2 5)))
(show (square (square 3)))
(define (sum-of-squares x y) (+ (square x) (square y)))
(show (sum-of-squares 3 4))
(define (f a) (sum-of-squares (+ a 1) (* a 2)))
(show (f 5))

;; 1.1.6 Conditional Expressions and Predicates
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(show (abs -3))
(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))
(show (abs2 -4))
(define (abs3 x)
  (if (< x 0)
      (- x)
      x))
(show (abs3 -5))

;; 1.1.7 Example: Square Roots by Newton's Method
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(show (sqrt 9))
(show (sqrt (+ 100 37)))
(show (sqrt (+ (sqrt 2) (sqrt 3))))
(show (square (sqrt 1000)))

;; 1.1.8 Procedures as Black-Box Abstractions (block structure)
(define (sqrt2 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(show (sqrt2 9))
