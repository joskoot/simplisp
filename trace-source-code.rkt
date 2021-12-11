#lang racket

(require simplisp/simplisp)

; WARNING
;
; Running this module produces almost 100000 lines of output.
; Maximum line length 200.

(simplisp
 '(trace-align '5)
 '(trace-width 90)
 '(trace-option 'all)
 source-code)

;(parameterize ((current-output-port (open-output-file "trace.txt" #:exists 'replace)))
; (begin0
;  (simplisp
;  '(trace-align '5)
;  '(trace-width 200)
;  '(trace-option 'all #;'(start finis selfi))
;   source-code)
;   (flush-output)
;  (close-output-port (current-output-port))))

;(parameterize ((current-output-port (open-output-nowhere)))
; (simplisp
; '(trace-align '5)
; '(trace-width 200)
; '(trace-option 'all #;'(start finis selfi))
; source-code))
