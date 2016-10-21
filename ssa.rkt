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

(define (fold-propagate-ssa ssa)
  (let ((next-ssa (map fold-line (propagate-constants ssa))))
    (if (equal? ssa next-ssa) ssa (fold-propagate-ssa next-ssa))))

(define (propagate-constants ssa)
  (let ((constants (filter (lambda (line) (equal? (third line)"#")) ssa)))
    (substitute-ssa ssa (map (lambda (line)
                               (list (list "%" (second line)) (list "#" (fourth line))))
                             constants))))

(define (fold-line line)
  (cond ((and (equal? (third line) "=") (equal? (first (fourth line)) "#") (equal? (first (fifth line)) "#"))
          (append (take line 2) (list "#" (equal? (second (fourth line)) (second (fifth line))))))
        ((and (equal? (third line) "if") (equal? (first (fourth line)) "#"))
         (append (take line 2) (if (second (fourth line)) (fifth line) (sixth line))))
         (else line)))

(define (substitute-ssa ssa subs)
  (if (= (length subs) 0)
    ssa
    (substitute-ssa (substitute-single ssa (first subs)) (rest subs))))

(define (substitute-single ssa sub)
  (map (lambda (line) 
         (map (lambda (phrase)
                (if (equal? phrase (first sub)) (second sub) phrase))
              line))
       ssa))

(define (reference? ssa)
  (equal? (first ssa) "%"))

(define (remove-dead-ssa ssa top)
  (let* ((refs (map second (remove-duplicates (filter reference? (filter list? (append-map cdddr ssa))))))
         (relevant (cons top refs)))
    (filter (lambda (line) (not (not (member (second line) relevant)))) ssa)))

; test ssa generation
; (pretty-print (first (generate-ssa '("goto" ("+" 1 2) ("*" 2 3)) '() 0)))
(let* ((out (generate-ssa '("if" ("=" 1 2) ("chain" ("turn" 1) ("move" 10)) ("chain" ("move" 5))) '() 0))
       (ssa (first out))
       (top (- (second out) 1)))
  (pretty-print (fold-propagate-ssa ssa)))
