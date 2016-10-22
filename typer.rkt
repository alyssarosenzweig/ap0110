#lang racket

(define types '(bool int float string stack))

(define (wildcard ssa)
  (map (lambda (line) (append line (list types))) ssa))

(define (type-immediate ssa)
  (map (lambda (line)
         (if (equal? (third line) "#")
           (append (take line 4) (cond ((integer? (fourth line)) '((int)))
                                        ((number? (fourth line)) '((float)))
                                        ((string? (fourth line)) '((string)))
                                        ((boolean? (fourth line)) '((bool)))
                                        (else '((und)))))
           line))
       ssa))

(define (member? lst mem) (not (not (member lst mem))))

(define (get-type ssa value)
  (if (equal? (first value) "%")
    (last (first (filter (lambda (k) (= (second k) (second value))) ssa)))
    '(und)))

; coerce types on both sides to match one of the possibilities

(define (fit-types ssa lst poss)
  (let ((typed (map (lambda (t) (map (lambda (k) (type-coerce ssa k t)) lst)) poss)))
    (let ((candidates (filter (lambda (k) (andmap list? k)) typed)))
      (if (> (length candidates) 0)
        (first candidates)
        '((und) ())))))

; coerce to type iff there is no loss of precision
(define (type-coerce ssa value type)
  (let ((now (get-type ssa value)))
    (cond ((and (= (length now) 1) (equal? (first now) type)) (list type value '()))
          (else              #f))))

(define (type-conditionals ssa)
  (append (map (lambda (line)
         (if (member? (third line) '("=" ">" "<"))
           (let ((fit (fit-types ssa (list (fourth line) (fifth line)) '(bool int float string))))
             (cons (append (drop-right line 1) (list (first fit)))
                   (second fit)))
           (list line)))
       ssa)))

(define (type-ssa ssa)
  (type-conditionals (type-immediate (wildcard ssa))))

(provide type-ssa)
