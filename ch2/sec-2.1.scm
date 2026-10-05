;; SICP 2.1 -- Introduction to Data Abstraction.
;; Code and stated results from Structure and Interpretation of Computer
;; Programs, 2nd ed., by Harold Abelson and Gerald Jay Sussman with Julie
;; Sussman (MIT Press), CC BY-SA 4.0. See ../NOTICE.
;;
;; print-rat starts with (newline), as in the book, so the output has a
;; blank line before each rational.  2.1.4's interval code is not run: its
;; constructor is Exercise 2.7's.  Exercise code is left out.
(import (scheme base) (scheme write) (srfi 216))

(define (show x) (display x) (newline))

;; 2.1.1 Example: Arithmetic Operations for Rational Numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 2))
(show (car x))                          ; 1
(show (cdr x))                          ; 2

(define y (cons 3 4))
(define z (cons x y))
(show (car (car z)))                    ; 1
(show (car (cdr z)))                    ; 3

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)                    ; 1/2
(define one-third (make-rat 1 3))
(print-rat
 (add-rat one-half one-third))          ; 5/6
(print-rat
 (mul-rat one-half one-third))          ; 1/6
(print-rat
 (add-rat one-third one-third))         ; 6/9

;; Reducing to lowest terms: add-rat picks up the new make-rat.
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))
(print-rat
 (add-rat one-third one-third))         ; 2/3
(newline)
(show (list (equal-rat? (sub-rat one-half one-third) (make-rat 1 6))
            (equal-rat? (div-rat one-half one-third) (make-rat 3 2))))

;; 2.1.2 Abstraction Barriers: reduce when selecting instead.
(define (make-rat n d)
  (cons n d))
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))
(print-rat
 (add-rat one-third one-third))         ; 2/3 again
(newline)

;; 2.1.3 What Is Meant by Data?  Pairs as procedures.
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else
           (error "Argument not 0 or 1:
                   CONS" m))))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))
(show (car (cons 1 2)))
(show (cdr (cons 1 2)))
