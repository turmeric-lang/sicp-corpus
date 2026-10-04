;; SICP 2.3 -- Symbolic Data.
;; Code and stated results from Structure and Interpretation of Computer
;; Programs, 2nd ed., by Harold Abelson and Gerald Jay Sussman with Julie
;; Sussman (MIT Press), CC BY-SA 4.0. See ../NOTICE.
;;
;; As in the book, later definitions replace earlier ones of the same name
;; (make-sum, the set operations, left-branch).  Exercise code, including
;; 2.64's list->tree and 2.67's sample tree, is left out; 2.3.4 instead
;; builds Figure 2.18's tree and decodes the message the text encodes with it.
(import (scheme base) (scheme write) (srfi 216))

(define (show x) (display x) (newline))

;; 2.3.1 Quotation
(define a 1)
(define b 2)
(show (list a b))                               ; (1 2)
(show (list 'a 'b))                             ; (a b)
(show (list 'a b))                              ; (a 2)
(show (car '(a b c)))                           ; a
(show (cdr '(a b c)))                           ; (b c)

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(show (memq 'apple '(pear banana prune)))       ; false
(show (memq 'apple '(x (apple sauce) y apple pear)))  ; (apple pear)

;; 2.3.2 Example: Symbolic Differentiation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression
                      type: DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(show (deriv '(+ x 3) 'x))                      ; (+ 1 0)
(show (deriv '(* x y) 'x))                      ; (+ (* x 0) (* 1 y))
(show (deriv '(* (* x y) (+ x 3)) 'x))
;; (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(show (deriv '(+ x 3) 'x))                      ; 1
(show (deriv '(* x y) 'x))                      ; y
(show (deriv '(* (* x y) (+ x 3)) 'x))          ; (+ (* x y) (* y (+ x 3)))

;; 2.3.3 Example: Representing Sets
;; Sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)
                                set2))))
(show (list (element-of-set? 3 '(1 3 5)) (adjoin-set 4 '(1 3 5)) (adjoin-set 3 '(1 3 5))
            (intersection-set '(1 3 5 7) '(7 5 2))))

;; Sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set
                          (cdr set1)
                          set2))
              ((< x2 x1) (intersection-set
                          set1
                          (cdr set2)))))))
(show (list (element-of-set? 4 '(1 3 5)) (intersection-set '(1 3 5 7) '(2 5 7 9))))

;; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set?
          x
          (left-branch set)))
        ((> x (entry set))
         (element-of-set?
          x
          (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))
;; Figure 2.16's first tree: 7 at the root, {3 1 5} left, {9 11} right.
(define tree-set
  (adjoin-set 11 (adjoin-set 9 (adjoin-set 5 (adjoin-set 1 (adjoin-set 3 (adjoin-set 7 '())))))))
(show tree-set)
(show (list (element-of-set? 5 tree-set) (element-of-set? 6 tree-set)))

;; 2.3.4 Example: Huffman Encoding Trees
;; Representing Huffman trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; The decoding procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits)
                current-branch)))
          (if (leaf? next-branch)
              (cons
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits)
                        next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit:
               CHOOSE-BRANCH" bit))))

;; Sets of weighted elements
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))
(show (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))

;; Figure 2.18's tree, built bottom-up as the merges in the text build it.
(define figure-2.18
  (make-code-tree
   (make-leaf 'A 8)
   (make-code-tree
    (make-code-tree (make-leaf 'B 3)
                    (make-code-tree (make-leaf 'C 1) (make-leaf 'D 1)))
    (make-code-tree (make-code-tree (make-leaf 'E 1) (make-leaf 'F 1))
                    (make-code-tree (make-leaf 'G 1) (make-leaf 'H 1))))))
(show (symbols figure-2.18))                    ; (A B C D E F G H)
(show (weight figure-2.18))                     ; 17
;; "100010100101101100011010100100000111001111" is BACADAEAFABBAAAGAH.
(show (decode '(1 0 0 0 1 0 1 0 0 1 0 1 1 0 1 1 0 0 0 1 1
                0 1 0 1 0 0 1 0 0 0 0 0 1 1 1 0 0 1 1 1 1)
              figure-2.18))
