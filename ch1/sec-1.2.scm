;; SICP 1.2 -- Procedures and the Processes They Generate.
;; Code and stated results from Structure and Interpretation of Computer
;; Programs, 2nd ed., by Harold Abelson and Gerald Jay Sussman with Julie
;; Sussman (MIT Press), CC BY-SA 4.0. See ../NOTICE.
;;
;; Left out: 1.2.1's two `+` procedures over inc/dec (they would replace `+`
;; for the rest of the program), and all exercise code.
(import (scheme base) (scheme write) (srfi 216))

(define (show x) (display x) (newline))

;; 1.2.1 Linear Recursion and Iteration
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(show (factorial 6))                    ; Figure 1.3: 720

(define (factorial-iterative n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
(show (factorial-iterative 6))          ; Figure 1.4: 720

;; 1.2.2 Tree Recursion
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(show (fib 5))                          ; Figure 1.5: 5

(define (fib-iterative n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
(show (fib-iterative 5))
;; "0, 1, 1, 2, 3, 5, 8, 13, 21, ..."
(show (map fib-iterative '(0 1 2 3 4 5 6 7 8)))

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0))
         0)
        (else
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination
                           kinds-of-coins))
                kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
(show (count-change 100))               ; 292

;; 1.2.4 Exponentiation
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
(show (expt 2 10))

(define (expt-linear-iterative b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))
(show (expt-linear-iterative 2 10))

(define (square x) (* x x))
(define (fast-expt b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
(show (fast-expt 2 10))
;; Integers do not overflow: all 31 digits of 2^100.
(show (fast-expt 2 100))

;; 1.2.5 Greatest Common Divisors
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(show (gcd 206 40))                     ; GCD(206,40) = 2

;; 1.2.6 Example: Testing for Primality
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(show (list (prime? 7) (prime? 91) (smallest-divisor 91)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else false)))
;; A prime always passes; 2^89-1 is a Mersenne prime, so this reaches
;; bignum arithmetic and a bignum `random`.
(show (fast-prime? 1009 10))
(show (fast-prime? 618970019642690137449562111 10))

;; Footnote: factorial with block structure
(define (factorial-block n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
(show (factorial-block 6))
