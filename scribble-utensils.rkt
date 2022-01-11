#lang scribble/manual

@;====================================================================================================

@(require
  scribble/core
  scribble/eval
  racket
  "simplisp.rkt"
  (for-label "simplisp.rkt" racket)
  (for-template "simplisp.rkt" racket)
  (for-syntax racket))

@(provide (all-defined-out))

@(define curdir (current-directory))

@(define-syntax-rule (Interaction x ...)
  (interaction #:eval (make-base-eval #:lang '(begin (require racket "simplisp.rkt")
                                                     (print-as-expression #f))) x ...))

@(define-syntax-rule (Interaction* x ...)
  (interaction #:eval evaller x ...))

@(define (make-evaller) (make-base-eval #:lang '(begin (require racket "simplisp.rkt")
                                                (print-as-expression #f))))

@(define evaller (make-evaller))

@(define (reset-Interaction*) (set! evaller (make-evaller)))

@(define-syntax-rule (Elemtag x) (add-elem-tag x))

@(define add-elem-tag (let ((tags '())) (λ ((x 'not-present))
                                           (cond
                                            ((eq? x 'not-present) tags)
                                            (else (set! tags (cons x tags))
                                                  (elemtag x))))))
@(define lb linebreak)
@(define nb nonbreaking)
@; ignore is a syntax such as to prevent arguments to be evaluated.
@(define-syntax-rule (ignore x ...) (void))
@; Below syntaxes are used such as to allow keyword arguments
@; without explicitly mentioning them in the definitions.
@(define-syntax-rule (nbsl x ...) (nb (seclink    x ...)))
@(define-syntax-rule (nbsr x ...) (nb (secref     x ...)))
@(define-syntax-rule (nbhl x ...) (nb (hyperlink  x ...)))
@(define-syntax-rule (nber x ...) (nb (elemref    x ...)))
@(define-syntax-rule (nbrl x ...) (nb (racketlink x ...)))
@(define-syntax-rule (nbr  x ...) (nb (racket     x ...)))
@(define-syntax-rule (nbpr x) (nber x (tt x)))
@(define-syntax-rule (defmacro x ...) (defform #:kind "macro" x ...))
@(define-syntax-rule (defmacro* x ...) (defform* #:kind "macro" x ...))
@(define (tt . content) (element 'tt (apply list content)))
@(define(minus) (tt "-"))
@(define(-?) (element "roman" ?-))
@(define (note . x) (inset (apply smaller x)))
@(define (inset . x) (apply nested #:style 'inset x))
@(define (expt-1) @↑{@(minus)1})
@(define ↑ superscript)
@(define ↓ subscript)
@(define-syntax-rule (Tabular ((e ...) ...) . rest) (tabular (list (list e ...) ...) . rest))
@(define (roman . x) (element 'roman x))
@(define (nbtt x) (nb (ttblack x)))
@(define test-arg   (make-parameter "yet to be constructed"))
@(define test-thunk (make-parameter "yet to be constructed"))
@(define test-block (make-parameter "yet to be constructed"))
@(define (get-simplisp-val var) (simplisp `,var))

@(define Void (let ((x
@seclink["void" #:doc '(lib "scribblings/reference/reference.scrbl") (nb (tt "#<void>"))]))
              (λ () x)))

@(define-syntax-rule (Tabular-with-linebreaks ((e ...) ... (le ...)) . rest)
  (Tabular (((list e (lb) (hspace 1)) ...) ... (le ...)) . rest))

@(define (make-color-style color)
  (define prop:color (color-property color))
  (define color-style (style #f (list prop:color)))
  (lambda elems (element 'roman (element color-style elems))))

@(define (make-ttcolor-style color)
  (define prop:color (color-property color))
  (define color-style (style #f (list prop:color)))
  (lambda elems (element 'tt (element color-style elems))))

@(define red       (make-color-style   "red"))
@(define green     (make-color-style   "green"))
@(define blue      (make-color-style   "blue"))
@(define black     (make-color-style   "black"))
@(define ttblack   (make-ttcolor-style "black"))
@(define ttred     (make-ttcolor-style "red"))
@(define ttgreen   (make-ttcolor-style "green"))
@(define optional "optional, evaluated, default: ")
@(define opt-proc "optional, default: ")

@(define (Rckt) (nbhl "https://docs.racket-lang.org/reference/index.html" "Racket"))

@(define (keyword . x)
  (apply seclink "keywords" #:doc '(lib "scribblings/reference/reference.scrbl") x)) 