;; SICP 1.3 -- Formulating Abstractions with Higher-Order Procedures.
;; Code and stated results from Structure and Interpretation of Computer
;; Programs, 2nd ed., by Harold Abelson and Gerald Jay Sussman with Julie
;; Sussman (MIT Press), CC BY-SA 4.0. See ../NOTICE.
;;
;; Where the book redefines a procedure (pi-sum, integral, f, sqrt), every
;; version is kept, each under its own name. Exercise code is left out.
;; The book prints .24998750000000042; Scheme's own printer writes the 0.
(import (scheme base) (scheme write) (scheme inexact) (srfi 216))

(define (show x) (display x) (newline))
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (cube x) (* x x x))

;; 1.3.1 Procedures as Arguments
(define (sum-integers-0 a b)
  (if (> a b)
      0
      (+ a (sum-integers-0 (+ a 1) b))))
(define (sum-cubes-0 a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes-0 (+ a 1) b))))
(define (pi-sum-0 a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum-0 (+ a 4) b))))
(show (list (sum-integers-0 1 10) (sum-cubes-0 1 10)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(show (sum-cubes 1 10))                         ; 3025

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(show (sum-integers 1 10))                      ; 55

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
(show (* 8 (pi-sum 1 1000)))                    ; 3.139592655589783

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(show (integral cube 0 1 0.01))                 ; .24998750000000042
(show (integral cube 0 1 0.001))                ; .249999875000001

;; 1.3.2 Constructing Procedures Using Lambda
(define (pi-sum-lambda a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))
(show (* 8 (pi-sum-lambda 1 1000)))
(define (integral-lambda f a b dx)
  (* (sum f (+ a (/ dx 2.0))
            (lambda (x) (+ x dx))
            b)
     dx))
(show (integral-lambda cube 0 1 0.01))

(define (plus4 x) (+ x 4))
(define plus4-lambda (lambda (x) (+ x 4)))
(show (list (plus4 1) (plus4-lambda 1)))

(show ((lambda (x y z) (+ x y (square z))) 1 2 3))   ; 12

(define (f-helper-version x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))
(define (f-lambda x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
(define (f-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(define (f-define x y)
  (define a
    (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))
;; The four agree.
(show (list (f-helper-version 2 3) (f-lambda 2 3) (f-let 2 3) (f-define 2 3)))

;; "if the value of x is 5, the value of the expression ... is 38"
(define x 5)
(show (+ (let ((x 3))
           (+ x (* x 10)))
         x))
;; "if the value of x is 2, the expression ... will have the value 12"
(define (let-scope x)
  (let ((x 3)
        (y (+ x 2)))
    (* x y)))
(show (let-scope 2))

;; 1.3.3 Procedures as General Methods
(define (search f neg-point pos-point)
  (let ((midpoint
         (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond
           ((positive? test-value)
            (search f neg-point midpoint))
           ((negative? test-value)
            (search f midpoint pos-point))
           (else midpoint))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value))
           (search f a b))
          ((and (negative? b-value)
                (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of
                   opposite sign" a b)))))
(show (half-interval-method sin 2.0 4.0))       ; 3.14111328125
(show (half-interval-method
       (lambda (x) (- (* x x x) (* 2 x) 3))
       1.0
       2.0))                                    ; 1.89306640625

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
;; The book prints .7390822985224023.  The last digit depends on the C
;; library's cos (macOS's gives ...023, glibc's, like Racket's, ...024), so
;; this prints whether the answer is the book's to within that digit.
(show (< (abs (- (fixed-point cos 1.0) .7390822985224023)) 1e-15))
(show (fixed-point (lambda (y) (+ (sin y) (cos y)))
                   1.0))                        ; 1.2587315962971173

;; The undamped (define (sqrt x) (fixed-point (lambda (y) (/ x y)) 1.0))
;; does not converge, as the book explains; it is not run.
(define (sqrt-damped x)
  (fixed-point
   (lambda (y) (average y (/ x y)))
   1.0))
(show (sqrt-damped 2))

;; 1.3.4 Procedures as Returned Values
(define (average-damp f)
  (lambda (x)
    (average x (f x))))
(show ((average-damp square) 10))               ; 55

(define (sqrt-average-damp x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))
(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y)
      (/ x (square y))))
   1.0))
(show (sqrt-average-damp 2))
(show (cube-root 27))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)
(show ((deriv cube) 5))                         ; 75.00014999664018

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))
(define (sqrt-newton x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0))
(show (sqrt-newton 2))

(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))
(define (sqrt-transform-damp x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))
(define (sqrt-transform-newton x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))
(show (sqrt-transform-damp 2))
(show (sqrt-transform-newton 2))
