#lang racket

; SSA emission
; the ap0110 compiler internally emits monads
; it's just easier this way :-)

(define (generate-ssa expr src base)
  (cond ((list? expr) (generate-ssa-list expr src base))
        ((number? expr) (generate-ssa-immediate expr src base))
        ((string? expr) (generate-ssa-immediate expr src base))))

(define (generate-ssa-immediate expr src base)
  (list (cons (list "=" base "#" expr) src) (+ base 1)))

(define (generate-ssa-list expr src base)
  (let ((arguments (generate-ssa-arguments (rest expr) src base '())))
    (let ((nsrc (first arguments))
          (nbase (second arguments))
          (args (third arguments)))
      (list (cons (append (list "=" nbase (first expr)) args) nsrc) (+ nbase 1)))))

(define (generate-ssa-arguments lst src base out)
  (if (= (length lst) 0)
    (list src base (reverse out))
    (let ((ssa (generate-ssa (first lst) src base)))
      (let ((nsrc (first ssa))
            (nbase (second ssa)))
      (generate-ssa-arguments (rest lst) nsrc nbase (cons (list "%" (- nbase 1)) out))))))

; SSA optimization passes

(define (inline-constants ssa)
  (let ((constants (filter (lambda (line) (equal? (third line)"#")) ssa)))
    (rename-ssa ssa (map (lambda (line)
                           (list (list "%" (second line)) (list "#" (fourth line))))
                         constants))))

(define (rename-ssa ssa substitutions)
  (if (= (length substitutions) 0)
    ssa
    (rename-ssa (rename-single ssa (first substitutions)) (rest substitutions))))

(define (rename-single ssa sub)
  (map (lambda (line) 
         (map (lambda (phrase)
                (if (equal? phrase (first sub)) (second sub) phrase))
              line))
       ssa))

; test ssa generation
; (pretty-print (first (generate-ssa '("goto" ("+" 1 2) ("*" 2 3)) '() 0)))
(let ((ssa (first (generate-ssa '("if" ("=" 1 2) ("mc" ("move" 10)) ("mc" ("move" 5))) '() 0))))
  (pretty-print (inline-constants ssa)))
