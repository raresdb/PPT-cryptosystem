#lang racket

(provide (all-defined-out))

;; PPT can be obtained in the form of a tree through the following
;; matrix transformations beginning with (3,4,5) as root.
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; where:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; In this representation, the triplets are indexed from up to down,
;; followed by left to right, as a result the order being:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; Scalar product of 2 vectors
(define (dot-product X Y)
  (if (null? X)
      0
      (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))
))

; Product of a matrix with a vector
(define (multiply M V)
  (if (not (list? (car M)))
      M
      (multiply (append (cdr M) (list (dot-product (car M) V))) V)
))

; Compute the sequence of transformations required to get the nth
; PPT from (3,4,5)
-
(define (get-transformations n)
  (cond
    [(not (list? n)) (get-transformations (list n))]
    [(equal? (car n) 1) (reverse (cdr n))]
    [else (get-transformations
       (append (cons (quotient (+ (car n) 1) 3) (cdr n)) (list (+ (remainder (- (car n) 2) 3) 1))))]
))

; Compute the PPT obtained by applying a given set of transformations
; to a given PPT as start
(define (apply-matrix-transformations Ts ppt)
  (if (null? Ts)
      ppt
      (apply-matrix-transformations (cdr Ts)
       (multiply
        (cond
            [(equal? (car Ts) 1) T1]
            [(equal? (car Ts) 2) T2]
            [(equal? (car Ts) 3) T3]
          )
        ppt)
)))

; Compute the nth PPT from the tree
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5))
)
