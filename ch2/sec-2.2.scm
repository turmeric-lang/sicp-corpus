;; SICP 2.2 -- Hierarchical Data and the Closure Property (2.2.1-2.2.3).
;; Code and stated results from Structure and Interpretation of Computer
;; Programs, 2nd ed., by Harold Abelson and Gerald Jay Sussman with Julie
;; Sussman (MIT Press), CC BY-SA 4.0. See ../NOTICE.
;;
;; The book's own list-ref, length, append, map and filter replace the
;; standard ones, as in the book, after the book has used the standard ones.
;; 2.2.4's picture language is out of scope (SRFI 216 defers it to SRFI 203),
;; and exercise code (the eight queens are Exercise 2.42) is left out.
;; fib, prime? and their helpers are chapter 1's, which 2.2.3 uses.
(import (scheme base) (scheme write) (srfi 216))

(define (show x) (display x) (newline))
(define (square x) (* x x))

;; From chapter 1.
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n) (= n (smallest-divisor n)))

;; 2.2.1 Representing Sequences
(show (cons 1
            (cons 2
                  (cons 3
                        (cons 4 nil)))))
(define one-through-four (list 1 2 3 4))
(show one-through-four)                         ; (1 2 3 4)
(show (car one-through-four))                   ; 1
(show (cdr one-through-four))                   ; (2 3 4)
(show (car (cdr one-through-four)))             ; 2
(show (cons 10 one-through-four))               ; (10 1 2 3 4)
(show (cons 5 one-through-four))                ; (5 1 2 3 4)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)
                (- n 1))))
(define squares
  (list 1 4 9 16 25))
(show (list-ref squares 3))                     ; 16

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds
  (list 1 3 5 7))
(show (length odds))                            ; 4

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a)
                     (+ 1 count))))
  (length-iter items 0))
(show (length odds))                            ; 4

(show (append squares odds))                    ; (1 4 9 16 25 1 3 5 7)
(show (append odds squares))                    ; (1 3 5 7 1 4 9 16 25)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))
(show (append squares odds))

;; Mapping over lists
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))
(show (scale-list (list 1 2 3 4 5) 10))         ; (10 20 30 40 50)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(show (map abs (list -10 2.5 -11.6 17)))        ; (10 2.5 11.6 17)
(show (map (lambda (x) (* x x)) (list 1 2 3 4)))  ; (1 4 9 16)

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(show (scale-list (list 1 2 3 4 5) 10))

;; 2.2.2 Hierarchical Structures
(show (cons (list 1 2) (list 3 4)))
(define x (cons (list 1 2) (list 3 4)))
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(show (length x))                               ; 3
(show (count-leaves x))                         ; 4
(show (list x x))                               ; (((1 2) 3 4) ((1 2) 3 4))
(show (length (list x x)))                      ; 2
(show (count-leaves (list x x)))                ; 8

;; Mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (* tree factor))
        (else
         (cons (scale-tree (car tree)
                           factor)
               (scale-tree (cdr tree)
                           factor)))))
(show (scale-tree (list 1
                        (list 2 (list 3 4) 5)
                        (list 6 7))
                  10))                          ; (10 (20 (30 40) 50) (60 70))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
(show (scale-tree (list 1
                        (list 2 (list 3 4) 5)
                        (list 6 7))
                  10))

;; 2.2.3 Sequences as Conventional Interfaces
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares
                  (car tree))
                 (sum-odd-squares
                  (cdr tree))))))
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))
(show (sum-odd-squares (list 1 (list 2 (list 3 4)) 5)))
(show (even-fibs 10))

(show (map square (list 1 2 3 4 5)))            ; (1 4 9 16 25)

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else  (filter predicate
                       (cdr sequence)))))
(show (filter odd? (list 1 2 3 4 5)))           ; (1 3 5)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))
(show (accumulate + 0 (list 1 2 3 4 5)))        ; 15
(show (accumulate * 1 (list 1 2 3 4 5)))        ; 120
(show (accumulate cons nil (list 1 2 3 4 5)))   ; (1 2 3 4 5)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))
(show (enumerate-interval 2 7))                 ; (2 3 4 5 6 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))
(show (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))  ; (1 2 3 4 5)

(define (sum-odd-squares tree)
  (accumulate
   +
   0
   (map square
        (filter odd?
                (enumerate-tree tree)))))
(define (even-fibs n)
  (accumulate
   cons
   nil
   (filter even?
           (map fib
                (enumerate-interval 0 n)))))
(show (sum-odd-squares (list 1 (list 2 (list 3 4)) 5)))
(show (even-fibs 10))

(define (list-fib-squares n)
  (accumulate
   cons
   nil
   (map square
        (map fib
             (enumerate-interval 0 n)))))
(show (list-fib-squares 10))                    ; (0 1 1 4 9 25 64 169 441 1156 3025)

(define
  (product-of-squares-of-odd-elements
   sequence)
  (accumulate
   *
   1
   (map square (filter odd? sequence))))
(show (product-of-squares-of-odd-elements
       (list 1 2 3 4 5)))                       ; 225

;; Nested Mappings
(define n 6)
(show (accumulate
       append
       nil
       (map (lambda (i)
              (map (lambda (j)
                     (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (flatmap
         (lambda (i)
           (map (lambda (j)
                  (list i j))
                (enumerate-interval
                 1
                 (- i 1))))
         (enumerate-interval 1 n)))))
;; The book's table for n = 6.
(show (prime-sum-pairs 6))

(define (permutations s)
  (if (null? s)   ; empty set?
      (list nil)  ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p))
                      (permutations
                       (remove x s))))
               s)))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
(show (permutations (list 1 2 3)))
