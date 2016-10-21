#lang racket

(define (generate-ssa expr src base)
  (cond ((list? expr) (generate-ssa-list expr src base))
        ((number? expr) (generate-ssa-immediate expr src base))
        ((string? expr) (generate-ssa-immediate expr src base))))

(define (generate-ssa-immediate expr src base)
  (list (cons (list "=" base expr) src) (+ base 1)))

(define (generate-ssa-list expr src base)
  (let ((arguments (generate-ssa-arguments (rest expr) src base)))
    (let ((nsrc (first arguments))
          (nbase (second arguments)))
      (list (cons (list "=" nbase expr) nsrc) (+ nbase 1)))))

(define (generate-ssa-arguments lst src base)
  (if (= (length lst) 0)
    (list src base)
    (let ((next (generate-ssa-arguments (rest lst) src base)))
      (let ((nsrc (first next))
            (nbase (second next)))
        (generate-ssa (first lst) nsrc nbase)))))

; test ssa generation
(generate-ssa '("move" ("+" 1 2)) '() 0)
