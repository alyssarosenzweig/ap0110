#lang racket

(require "ssa.rkt")
(require "ssa-optimization.rkt")
(require "typer.rkt")

(let* ((out (generate-ssa '("if" ("=" 1 2) ("chain" ("turn" 1) ("move" 10)) ("chain" ("move" 5))) '() 0))
       (ssa (first out))
       (top (- (second out) 1)))
  (pretty-print (type-ssa ssa)))
  ;(pretty-print (remove-dead-pass (fold-propagate-pass ssa) top)))
