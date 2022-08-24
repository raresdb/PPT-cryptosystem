#lang racket

(require "ppt.rkt")

(provide (all-defined-out))

; functions used for generating the key
(define (a1 e f) (+ (* e e) (* 2 f e)))
(define (b1 e f) (+ (* 2 f f) (* 2 f e)))
(define (c1 e f) (+ (* 2 f f) (* e e) (* 2 f e)))

(define (a2 e f) (- (* e e) (* 2 f e)))
(define (b2 e f) (- (* 2 f f) (* 2 f e)))
(define (c2 e f) (+ (* 2 f f) (* e e) (* -2 f e)))

(define (a3 e f) (- (* f f) (* e e)))
(define (b3 e f) (* 2 f e))
(define (c3 e f) (+ (* f f) (* e e)))

;; Messages consist of english letters and space.
;; Each character will be given a value:
;; space is 0
;; a,b,c.. will be 1,2,3..
;;
;; A number n will be used to generate the encryption key.
;; e and f values from the nth quadruple will be used to generate
;; the following key:
;;(a1,b1,c1,a2,b2,c2,a3,b3,c3) mod 27

; Generate the key from n.
(define (key n)
  (let* ([quad (get-nth-quadruple n)] [e (cadr quad)] [f (caddr quad)])
    (map (λ (x) (modulo x 27)) (foldr ( λ (func acc) (cons (func e f) acc))
                                   null (list a1 b1 c1 a2 b2 c2 a3 b3 c3)
                                   )
         )
    )
  )

; Convert the message into a 0-26 numeric code.
(define (message->codes message)
  (let ([sp 32])
    (map (λ (x)
           (let ([x (char->integer x)])
             (if (equal? x sp) (- x 32) (- x 96))
             )
           )
           (string->list message)
         )
    )
  )


; Get message from codes.
(define (codes->message codes)
    (list->string (map (λ (x) (if (equal? x 0) (integer->char (+ x 32))
                                  (integer->char (+ x 96))
                                  )
                         )
                       codes)
                  )
  )

;;    m = original message code
;;    c = crypted message code
;;    k = key code
;;
;; Pentru a putea efectua operații de criptare/decriptare,
;; cheia trebuie extinsă/trunchiată la dimensiunea
;; mesajului care trebuie criptat/decriptat.
;; For encryption the key must be extended or truncated to the
;; size of the message.
;;
;; Next step is to use the following formulas for each index:
;;   c[i] = (m[i] + k[i]) mod 27   -> For encryption.
;;
;;   m[i] = (c[i] - k[i]) mod 27   -> For decryption.

; Extend/truncate key
(define (extend-key key size)
  (let extend-aux ((lgth (length key)) (new-key key))
    (cond
      [(> size lgth) (extend-aux (+ lgth (length key)) (append new-key key))]
      [(< size lgth) (reverse (let cut-elements ((l (reverse new-key)) (lgth lgth))
                            (if (equal? lgth size)
                                l
                                (cut-elements (cdr l) (- lgth 1))
                                )
                            )
                          )
                     ]
      [new-key]
        )
    )
  )

;; Generic function for encryption/decryption of codes.
;;option must be+-1
;+ is for encryption
;- is for decryption
(define (encryption-agent array key option)
  (if (or (equal? option 1) (equal? option -1))
     (map (λ (a k) (modulo (+ a (* k option)) 27)) array (extend-key key (length array)))
     (error "Argument 3: expected +1 or -1, given:" option)
    )
  )

;; Specific functions for encryption/decryption of codes.
(define (encrypt-codes message key)
  (encryption-agent message key 1)
  )
(define (decrypt-codes encrypted key)
  (encryption-agent encrypted key -1)
  )

;; Specific functions for encryption/decryption of string messages.
(define (encrypt-message message key)
  (codes->message (encrypt-codes (message->codes message) key)
   )
  )

(define (decrypt-message encrypted key)
  (codes->message (decrypt-codes (message->codes encrypted) key)
   )
  )
           