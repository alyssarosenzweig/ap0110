#lang racket

(require math/matrix)

(define (member? el lst) (not (not (member el lst))))

(define (symbol->type sym)
  (case sym ((bool) 0)
            ((int) 1)
            ((float) 2)
            ((string) 3)
            ((stack) 4)
            ((undefined) -1)))

(define (immediate-type imm)
  (cond ((integer? imm) (symbol->type 'int))
        ((number?  imm) (symbol->type 'float))
        ((string?  imm) (symbol->type 'string))
        (else           (symbol->type 'undefined))))

(define (assert-immediate l)
  (list (list (list 1 (list "%" (second l))) (immediate-type (fourth l)))))

(define (assert-arithm l)
  (list (list (list  1 (list "%" (second l))
                    -1 (fourth l)
                    -1 (list "K" (second l)))
              0)
        (list (list  1 (list "%" (second l))
                    -1 (fifth l))
              0)))

(define assertions
  (hash "#" assert-immediate
        "+" assert-arithm
))

(define (assert-ssa ssa)
  (append-map (lambda (line) ((hash-ref assertions (third line)) line)) ssa))

(define (prepare-matlist asserts p)
  (remove-duplicates
    (append-map (lambda (ln) (map second (filter p (filter list? (first ln))))) asserts)))

(define (coefficient c types casts)
  (if (>= c (length types))
    (list "K" (list-ref casts (- c (length types))))
    (list "%" (list-ref types c))))

(define (get-coefficient var line)
  (if (< (length line) 2)
    0
    (if (equal? (second line) var)
      (first line)
      (get-coefficient var (cddr line)))))

(define (prepare-matrix asserts types casts)
  (build-matrix (length asserts) (+ (length types) (length casts))
                (lambda (x y)
                  (get-coefficient (coefficient x types casts) (first (list-ref asserts y))))))

(define (assert-solve asserts)
  (let ((types (prepare-matlist asserts (lambda (p) (equal? (first p) "%"))))
        (casts (prepare-matlist asserts (lambda (p) (equal? (first p) "K")))))
    (let ((mat (prepare-matrix asserts types casts)))
      mat)))

(define (type-ssa ssa)
  (assert-solve (assert-ssa ssa)))

(provide type-ssa)
