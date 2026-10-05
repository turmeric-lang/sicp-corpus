;; SICP 3.1 -- Assignment and Local State.
;; Code and stated results from Structure and Interpretation of Computer
;; Programs, 2nd ed., by Harold Abelson and Gerald Jay Sussman with Julie
;; Sussman (MIT Press), CC BY-SA 4.0. See ../NOTICE.
;;
;; Values are printed with `write`, so "Insufficient funds" keeps its
;; quotes as in the book.  3.1.2 leaves rand-update and random-init to the
;; reader (footnote 6 sketches a linear congruential generator); one is
;; supplied below, marked as not the book's.  Exercise code is left out.
(import (scheme base) (scheme write) (scheme inexact) (srfi 216))

(define (show x) (write x) (newline))

;; 3.1.1 Local State Variables
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
(show (withdraw 25))                            ; 75
(show (withdraw 25))                            ; 50
(show (withdraw 60))                            ; "Insufficient funds"
(show (withdraw 15))                            ; 35

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance
                       (- balance amount))
                 balance)
          "Insufficient funds"))))
(show (new-withdraw 25))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds")))
(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(show (W1 50))                                  ; 50
(show (W2 70))                                  ; 30
(show (W2 40))                                  ; "Insufficient funds"
(show (W1 40))                                  ; 10

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request:
                 MAKE-ACCOUNT" m))))
  dispatch)
(define acc (make-account 100))
(show ((acc 'withdraw) 50))                     ; 50
(show ((acc 'withdraw) 60))                     ; "Insufficient funds"
(show ((acc 'deposit) 40))                      ; 90
(show ((acc 'withdraw) 60))                     ; 30
(define acc2 (make-account 100))
(show ((acc2 'withdraw) 10))                    ; a separate account: 90

;; 3.1.2 The Benefits of Introducing Assignment
;; Not the book's: rand-update as a Park-Miller multiplicative generator.
(define random-init 7)
(define (rand-update x) (modulo (* x 16807) 2147483647))

(define rand
  (let ((x random-init))
    (lambda () (set! x (rand-update x)) x)))
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials
                          cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))
;; Close to pi, not equal to it.
(show (< (abs (- (estimate-pi 10000) 3.14159)) 0.1))

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials
                              random-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining
                trials-passed
                x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))
(show (< (abs (- (estimate-pi 10000) 3.14159)) 0.1))

;; 3.1.3 The Costs of Introducing Assignment
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
(define W (make-simplified-withdraw 25))
(show (W 20))                                   ; 5
(show (W 10))                                   ; -5

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))
(define D (make-decrementer 25))
(show (D 20))                                   ; 5
(show (D 10))                                   ; 15
(show ((make-decrementer 25) 20))               ; 5
(show ((lambda (amount) (- 25 amount)) 20))     ; 5
(show ((make-simplified-withdraw 25) 20))       ; 5

;; Sameness and change
(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))
(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))
(show (W1 20))                                  ; 5
(show (W1 20))                                  ; -15
(show (W2 20))                                  ; 5

(define peter-acc (make-account 100))
(define paul-acc (make-account 100))
((peter-acc 'withdraw) 10)
(show ((paul-acc 'withdraw) 0))                 ; distinct: 100
(define peter-acc (make-account 100))
(define paul-acc peter-acc)
((peter-acc 'withdraw) 10)
(show ((paul-acc 'withdraw) 0))                 ; the same account: 90

;; Pitfalls of imperative programming
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))
(show (factorial 6))
(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter
                                  product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))
(show (factorial 6))
