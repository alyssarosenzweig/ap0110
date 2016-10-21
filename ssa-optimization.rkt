#lang racket

(define (fold-propagate-pass ssa)
  (let ((next-ssa (map fold-line (propogate-references (propagate-constants ssa)))))
    (if (equal? ssa next-ssa) ssa (fold-propagate-pass next-ssa))))

(define (propagate-constants ssa)
  (let ((constants (filter (lambda (line) (equal? (third line) "#")) ssa)))
    (substitute-ssa ssa (map (lambda (line)
                               (list (list "%" (second line)) (list (third line) (fourth line))))
                             constants))))

(define (propogate-references ssa)
  (map (lambda (line)
         (if (equal? (third line) "%")
           (append (take line 2) (lookup ssa (fourth line)))
           line))
       ssa))

(define (lookup ssa number)
  (drop (first (filter (lambda (k) (= (second k) number)) ssa)) 2))

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
         (if (equal? (drop line 2) (first sub))
           (append (take line 2) (second sub))
           (map (lambda (phrase)
                  (if (equal? phrase (first sub)) (second sub) phrase))
                line)))
       ssa))

(define (reference? ssa)
  (equal? (first ssa) "%"))

(define (remove-dead-pass ssa top)
  (let ((next (remove-dead-ssa ssa top)))
    (if (equal? next ssa) ssa (remove-dead-pass next top))))

(define (remove-dead-ssa ssa top)
  (let* ((refs (map second (remove-duplicates (filter reference? (filter list? (append-map cdddr ssa))))))
         (relevant (cons top refs)))
    (filter (lambda (line) (not (not (member (second line) relevant)))) ssa)))

(provide remove-dead-pass)
(provide fold-propagate-pass)
