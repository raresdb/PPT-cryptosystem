#lang racket

(provide (all-defined-out))

;; The PPT tree can be computed by using the GH (Gopal-Hemachandra)
;; tuples.
;;
;; For a pair (g, e), the GH sequence is:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; The 4 numbers in the sequence from the GH quadruple:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; For such a quadruple(g, e, f, h), are defined:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; (a,b,c) being a PT.
;;
;; (a,b,c) become a PTT if the following conditions are met:
;;    g, e, f, h relatively prime among each other
;;    g uneven
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; The resulting tree look like this:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; check ppt1
(define (dot-product X Y)
  (apply + (foldl (lambda (x accL)
           (append (cdr accL) (list (* x (car accL)))))
                    X Y); Am produs lista de produse 
         )
  )

; Check ppt1
(define (multiply M V)
  (map ((curry dot-product) V) M)
  )

; Check ppt1
(define (get-transformations n)
  (cond
    [(not (list? n)) (get-transformations (list n))]
    [(equal? (car n) 1) (reverse (cdr n))]
    [else (get-transformations
       (append (cons (quotient (+ (car n) 1) 3) (cdr n)) (list (+ (remainder (- (car n) 2) 3) 1))))]
))

; Apply a sequence of transformations on a tuple
(define (apply-functional-transformations Fs tuple)
  (foldl (lambda (f acc-tuple)
           (f acc-tuple))
         tuple Fs)
  )

; Get nth tuple by using the set of transformations given
; They can be either T1,T2,T3 or Q1,Q2,Q3, or anything else
(define (get-nth-tuple n Fs tuple)
  (apply-functional-transformations (map ((curry list-ref) Fs) (map ((curry +) -1) (get-transformations n))) tuple)
  )

; Compute the nth triple
(define (get-nth-ppt-from-matrix-transformations n)
  (get-nth-tuple n (map (curry multiply) (list T1 T2 T3)) '(3 4 5))
  )

; Compute the nth quadruple
(define (get-nth-quadruple n)
  (get-nth-tuple n (map (lambda (q) (lambda (tuple) (apply q tuple))) (list Q1 Q2 Q3)) '(1 1 2 3))
  )

; Using the nth quadruple, compute the nth triple
(define (get-nth-ppt-from-GH-quadruples n)
  (let* ([quad (get-nth-quadruple n)] [e (cadr quad)] [f (caddr quad)])
    (list (* (car quad) (cadddr quad)) (* 2 e f) (+ (* e e) (* f f)))
    )
  )