;; SICP 3.4 -- Concurrency: Time Is of the Essence.
;; Code and stated results from Structure and Interpretation of Computer
;; Programs, 2nd ed., by Harold Abelson and Gerald Jay Sussman with Julie
;; Sussman (MIT Press), CC BY-SA 4.0. See ../NOTICE.
;;
;; parallel-execute and test-and-set! are SRFI 216's.  A concurrent run's
;; answer varies, so what is printed is whether every answer, over many
;; runs, is one the book allows -- never the answer itself.  The book's
;; closing test-and-set! is not defined here: as written it is not atomic,
;; and the book goes on to say a real one must be (footnote 47); SRFI 216's
;; is.  Exercise code is left out.
(import (scheme base) (scheme write) (srfi 216))

(define (show x) (display x) (newline))

;; 3.4 and 3.4.1: withdraw, one process
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin
        (set! balance
              (- balance amount))
        balance)
      "Insufficient funds"))
(show (withdraw 25))                            ; 75
(show (withdraw 25))                            ; 50

;; Implementing serializers (defined first: everything below uses them)
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))

;; Serializers in Scheme: x ends at 101, 121, 110, 11 or 100; serialized,
;; at 101 or 121.
(define (unserialized-once)
  (define x 10)
  (parallel-execute (lambda () (set! x (* x x)))
                    (lambda () (set! x (+ x 1))))
  x)
(define (serialized-once)
  (define x 10)
  (define s (make-serializer))
  (parallel-execute
   (s (lambda () (set! x (* x x))))
   (s (lambda () (set! x (+ x 1)))))
  x)
(define (always? n allowed run)
  (let loop ((i 0))
    (cond ((= i n) #t)
          ((memv (run) allowed) (loop (+ i 1)))
          (else #f))))
(show (always? 50 '(101 121 110 11 100) unserialized-once))
(show (always? 50 '(101 121) serialized-once))

;; A serialized account: concurrent deposits and withdrawals add up.
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
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw)
             (protected withdraw))
            ((eq? m 'deposit)
             (protected deposit))
            ((eq? m 'balance)
             balance)
            (else (error "Unknown request:
                          MAKE-ACCOUNT"
                         m))))
    dispatch))
(define acc (make-account 100))
(define (repeat n thunk) (if (> n 0) (begin (thunk) (repeat (- n 1) thunk))))
(parallel-execute (lambda () (repeat 200 (lambda () ((acc 'deposit) 1))))
                  (lambda () (repeat 100 (lambda () ((acc 'withdraw) 1))))
                  (lambda () (repeat 200 (lambda () ((acc 'deposit) 2)))))
(show (acc 'balance))                           ; 100 + 200 - 100 + 400 = 600

;; Complexity of using multiple shared resources
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer)
             balance-serializer)
            (else (error "Unknown request:
                          MAKE-ACCOUNT"
                         m))))
    dispatch))
(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

;; "Peter exchanges a1 and a2 while Paul exchanges a1 and a3": with the
;; exchanges serialized the balances stay $10, $20 and $30 in some order.
;; Both lock a1 first, so this pair cannot deadlock.
(define (exchange-once)
  (define a1 (make-account-and-serializer 10))
  (define a2 (make-account-and-serializer 20))
  (define a3 (make-account-and-serializer 30))
  (parallel-execute (lambda () (serialized-exchange a1 a2))
                    (lambda () (serialized-exchange a1 a3)))
  (let ((bs (list (a1 'balance) (a2 'balance) (a3 'balance))))
    (and (memv 10 bs) (memv 20 bs) (memv 30 bs) #t)))
(show (always? 50 '(#t) exchange-once))
(define a (make-account-and-serializer 5))
(deposit a 10)
(show (a 'balance))                             ; 15
