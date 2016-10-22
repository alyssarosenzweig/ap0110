#lang racket

(define types '(int float string stack))

(define (wildcard ssa)
  (map (lambda (line) (append line (list types))) ssa))

(define (type-immediate ssa)
  (map (lambda (line)
         (if (equal? (third line) "#")
           (append (take line 3) (cond ((integer? (fourth line)) '((int)))
                                        ((number? (fourth line)) '((float)))
                                        ((string? (fourth line)) '((string)))
                                        (else '((und)))))
           line))
       ssa))

(define (type-ssa ssa)
  (type-immediate (wildcard ssa)))

(provide type-ssa)
