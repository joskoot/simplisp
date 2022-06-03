#lang racket

(require simplisp/simplisp)

; WARNING
;
; Running this module produces almost 100000 lines of output.
; Maximum line length 90.

(if (eq? (system-type 'vm) 'racket)
 ; For racket bc  
 (simplisp
  '(trace-align '5)
  '(trace-width 90)
  '(trace-option 'all)
  source-code)
; For racket cs
(simplisp
 '(trace-align '5)
 '(trace-width 90)
 '(trace-option '(start finis selfi))
 source-code))

