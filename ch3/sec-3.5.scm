;; SICP 3.5 -- Streams.
;; Code and stated results from Structure and Interpretation of Computer
;; Programs, 2nd ed., by Harold Abelson and Gerald Jay Sussman with Julie
;; Sussman (MIT Press), CC BY-SA 4.0. See ../NOTICE.
;;
;; cons-stream comes from SRFI 216.  Departures, each marked below:
;;   - display-stream on an infinite stream is printed for its first terms;
;;   - 3.5.1's model of delay and force (which would replace the real force)
;;     runs inside a procedure;
;;   - add-streams is written directly over two streams: the book builds it
;;     on Exercise 3.50's general stream-map, an exercise answer;
;;   - pi-stream and its accelerations need partial-sums, Exercise 3.55, and
;;     are left out, as is all other exercise code;
;;   - the list-based (car (cdr (filter prime? (enumerate-interval 10000
;;     1000000)))) the book calls grossly inefficient is not run.
(import (scheme base) (scheme write) (srfi 216))

(define (show x) (display x) (newline))
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

;; 3.5.1 Streams Are Delayed Lists
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc
                         (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))
(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))

;; Not the book's: the first n terms, for infinite streams.
(define (stream-head s n)
  (if (= n 0) '() (cons (stream-car s) (stream-head (stream-cdr s) (- n 1)))))

;; The stream implementation in action
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream
          (stream-car stream)
          (stream-filter
           pred
           (stream-cdr stream))))
        (else (stream-filter
               pred
               (stream-cdr stream)))))
;; Chapter 1's prime?, which the section uses.
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n) (= n (smallest-divisor n)))

;; The second prime in [10000, 1000000]: 10009, after 10007.
(show (stream-car
       (stream-cdr
        (stream-filter
         prime? (stream-enumerate-interval
                 10000 1000000)))))
(display-stream (stream-enumerate-interval 1 3))
(newline)

;; Implementing delay and force: the book's model, kept to itself.
(define (delay-and-force-model)
  (define (force delayed-object)
    (delayed-object))
  (define (memo-proc proc)
    (let ((already-run? false) (result false))
      (lambda ()
        (if (not already-run?)
            (begin (set! result (proc))
                   (set! already-run? true)
                   result)
            result))))
  (define runs 0)
  (define p (memo-proc (lambda () (set! runs (+ runs 1)) 'value)))
  (list (force p) (force p) runs))
(show (delay-and-force-model))                  ; (value value 1)

;; 3.5.2 Infinite Streams
(define (integers-starting-from n)
  (cons-stream
   n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x)
                   (not (divisible? x 7)))
                 integers))
(show (stream-ref no-sevens 100))               ; 117

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
(show (stream-head fibs 10))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible?
                   x (stream-car stream))))
           (stream-cdr stream)))))
(define primes
  (sieve (integers-starting-from 2)))
(show (stream-ref primes 50))                   ; 233

;; Defining streams implicitly
(define ones (cons-stream 1 ones))
;; Not the book's: add-streams without Exercise 3.50's stream-map.
(define (add-streams s1 s2)
  (cons-stream (+ (stream-car s1) (stream-car s2))
               (add-streams (stream-cdr s1) (stream-cdr s2))))
(define integers
  (cons-stream 1 (add-streams ones integers)))
(show (stream-head integers 10))
(define fibs
  (cons-stream
   0 (cons-stream
      1 (add-streams
         (stream-cdr fibs) fibs))))
(show (stream-head fibs 10))                    ; 0 1 1 2 3 5 8 13 21 34

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))
(define double
  (cons-stream 1 (scale-stream double 2)))
(show (stream-head double 10))                  ; 1 2 4 8 ...

(define primes
  (cons-stream
   2 (stream-filter
      prime? (integers-starting-from 3))))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))
(show (stream-ref primes 50))                   ; 233 again

;; 3.5.3 Exploiting the Stream Paradigm
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)
;; 1. 1.5 1.4166666666666665 1.4142156862745097 1.4142135623746899
(for-each show (stream-head (sqrt-stream 2) 5))

;; Infinite streams of pairs
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (stream-append (stream-cdr s1) s2))))
(show (stream-head (stream-append (stream-enumerate-interval 1 2) integers) 4))
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
(show (stream-head (pairs integers integers) 6))
;; "the stream of all pairs of integers (i, j) with i <= j such that i + j
;; is prime"
(show (stream-head (stream-filter
                    (lambda (pair)
                      (prime? (+ (car pair) (cadr pair))))
                    (pairs integers integers))
                   4))

;; Streams as signals
(define (integral integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)
(show (stream-head (integral ones 0 1) 5))      ; 0 1 2 3 4

;; 3.5.4 Streams and Delayed Evaluation
(define (integral
         delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand
            (force delayed-integrand)))
       (add-streams
        (scale-stream integrand dt)
        int))))
  int)
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
;; The book shows 2.716924, rounded; e is 2.71828...
(show (stream-ref
       (solve (lambda (y) y) 1 0.001) 1000))
