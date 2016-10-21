#lang racket

(define (generate-ssa expr src base)
  (cond ((list? expr) (generate-ssa-list expr src base))
        ((number? expr) (generate-ssa-immediate expr src base))
        ((string? expr) (generate-ssa-immediate expr src base))))

(define (generate-ssa-immediate expr src base)
  (list (cons (list "=" base (list "#" expr)) src) (+ base 1)))

(define (generate-ssa-list expr src base)
  (let ((arguments (generate-ssa-arguments (rest expr) src base '())))
    (let ((nsrc (first arguments))
          (nbase (second arguments))
          (args (third arguments)))
      (list (cons (list "=" nbase (cons (first expr) args)) nsrc) (+ nbase 1)))))

(define (generate-ssa-arguments lst src base out)
  (if (= (length lst) 0)
    (list src base (reverse out))
    (let ((ssa (generate-ssa (first lst) src base)))
      (let ((nsrc (first ssa))
            (nbase (second ssa)))
      (generate-ssa-arguments (rest lst) nsrc nbase (cons (list "%" (- nbase 1)) out))))))

; test ssa generation
(pretty-print (generate-ssa '("goto" ("+" 1 2) ("*" 2 3)) '() 0))
