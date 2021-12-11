#lang racket

(require "simplisp.rkt" #;simplispbc/simplisp
 (for-syntax racket
  (only-in syntax/stx stx-pair?)
  (only-in "simplisp.rkt" source-code) racket))

; Find all required variables of simplisp.
; Not all racket forms are included, only those needed for simplisp.
; For example the source code of simplisp does define, but does not use quasiquote,
; hence we don't need to trace quasiquote-forms.

(define-syntax (req-vars stx) #;(show stx)
 (syntax-case stx
  (lambda λ let let* letrec let-values let*-values letrec-values let/cc let/ec
   quote quasiquote case else set!)
  ((_ id) (identifier? #'id) #''(id))
  ((_ x) (not (stx-pair? #'x)) #''())
  ((_ (lambda (var ...) expr ...))
 #'(rcons 'lambda (rem '(var ...) (append (req-vars expr) ...))))
  ((_ (lambda (var ... . rest-var) expr ...))
 #'(rcons 'lambda (rem '(var ... rest-var) (append (req-vars expr) ...))))
  ((_ (λ (var ...) expr ...))
 #'(rcons 'λ (rem '(var ...) (append (req-vars expr) ...))))
  ((_ (λ (var ... . rest-var) expr ...))
 #'(rcons 'λ (rem '(var ... rest-var) (append (req-vars expr) ...))))
  ((_ (let ((var expr) ...) body ...))
 #'(rcons 'let (append (req-vars expr) ... (rem '(var ...) (append (req-vars body) ...)))))
  ((_ (let id ((var expr) ...) body ...)) (identifier? #'id)
 #'(rcons 'let (append (req-vars expr) ... (rem '(id var ...) (append (req-vars body) ...)))))
  ((_ (let* () body ...)) #'(rcons 'let* (append (req-vars body) ...)))
  ((_ (let* ((arg expr) rest ...) body ...))
 #'(rcons 'let* (append (req-vars expr) (rem 'arg (req-vars (let* (rest ...) body ...))))))
  ((_ (letrec ((var expr) ...) body ...))
 #'(rcons 'letrec (rem '(var ...) (append (req-vars expr) ... (req-vars body ...)))))
  ((_ (let-values (((var ...) expr) ...) body ...))
 #'(rcons 'let-values (append (req-vars expr) ... (rem '(var ... ...) (req-vars (body ...))))))
  ((_ (let*-values () body ...)) #'(rcons 'let-values (append (req-vars body) ...)))
  ((_ (let*-values (((var ...) expr) rest ...) body ...))
 #'(rcons 'let*-values
    (append (req-vars expr) (rem '(var ...) (req-vars (let*-values (rest ...) body ...))))))
  ((_ (letrec-values (((var ...) expr) ...) body ...))
 #'(rcons 'letrec-values (rem '(var ... ...) (append (req-vars expr) ... (req-vars body) ...))))
  ((_ (case expr ((datum ...) body ...)))
 #'(rcons 'case (append (req-vars expr) (req-vars body) ...))) 
  ((_ (case expr (any body ...) ...))
 #'(rcons 'case (append (req-vars expr) (req-vars body) ... ...)))
  ((_ (quasiquote . x)) (raise-syntax-error 'req-vars "does not handle quasiquote forms" stx stx))
  ((_ (let/cc cc body ...)) #'(rcons 'let/cc (rem 'cc (append (req-vars body) ...))))
  ((_ (let/ec ec body ...)) #'(rcons 'let/ec (rem 'ec (append (req-vars body) ...))))
  ((_ (set! var expr)) #'(req-vars expr))
  ((_ (quote x)) #''(quote))
  ((_ (body . rest)) #'(rappend (req-vars body) (req-vars rest)))
  (else #''())))

(define-syntax (required-vars stx)
 (syntax-case stx ()
  ((_) #`(req-vars  #,source-code))))

(define (rappend . x) (remove-duplicates (apply append x) eq?))
(define (rcons a b) (remove-duplicates (cons a b) eq?))
(define (rem s lst) (remove* (if (symbol? s) (list s) s) lst))

(define (xprintf fmt data)
 (printf fmt (length data))
 (let loop ((n 0) (data data))
  (cond
   ((null? data) (printf "~n~n"))
   (else
    (let* ((str (~s (car data))) (m (+ 2 (string-length str))) (n (+ n m)))
     (cond
      ((> n 100) (newline) (printf "  ~a" str) (loop m (cdr data)))
      (else (printf "  ~a" str) (loop n (cdr data)))))))))

(define meta-req-vars (required-vars))
(define clean-vars (simplisp '(clean-vars)))
(define special-vars (simplisp '(special-vars)))
(define borrowed-vars (remove* special-vars clean-vars))
(newline)
(xprintf "Required variables for meta-recursivity: ~s~n~n" (sort meta-req-vars symbol<?))
(xprintf "Special variables: ~s~n~n" special-vars)

(printf "Check that all variables required for meta-recursivity are present~n~n")
(let
 ((missing-vars
   (for/fold ((missing '())) ((var (in-list meta-req-vars)))
    (with-handlers ((exn:fail? (λ (exn) (cons var missing))))
     (simplisp var) missing))))
 (if (null? missing-vars) (printf "Ok~n~n") (xprintf "~s vars missing:~n~n" missing-vars)))

(xprintf "Variables required and not special: ~s~n~n" (remove* special-vars meta-req-vars))
(xprintf "Variables special and not required: ~s~n~n" (remove* meta-req-vars special-vars))
(xprintf "Variables both special and required: ~s~n~n"
 (sort (set->list (set-intersect meta-req-vars special-vars)) symbol<?))

(printf "Nr of clean variables ~s~n~n" (length clean-vars))
(xprintf "Clean variables: ~s~n~n" clean-vars)
(xprintf "Borrowed variables: ~s~n~n" borrowed-vars)

(define (sexpr? x)
 (let/ec ec
  (define (sexpr? x)
   (or
    (null? x)
    (boolean? x)
    (string? x)
    (char? x)
    (exact-nonnegative-integer? x)
    (and (symbol? x) (not (member x '(quasiquote unquote unquote-splicing))))
    (member x '('quasiquote 'unquote 'unquote-splicing))
    (and (pair? x) (sexpr? (car x)) (sexpr? (cdr x)))
    (ec (begin (fprintf (current-error-port) "sexpr-test fails for: ~s~n" x) #f))))
  (sexpr? x)))

(if (sexpr? source-code)
 (printf "Source-code is a symbolic expression without quasiquotation.~n~n")
 (fprintf (current-error-port)
          "Source-code is NOT a symbolic expression without quasiquotation.~n~n"))

(define (type-nr x)
 (cond
  ((null? x) 0)
  ((boolean? x) (if x 2 1))
  ((number? x) 3)
  ((char? x) 4)
  ((string? x) 5)
  ((symbol? x) 6)
  (else (error 'type-nr "~s" x))))

(xprintf "All atoms of the source code (~s distinct atoms)~n~n"
(sort
 (remove-duplicates
  (let loop ((x source-code))
   (remove-duplicates
    (cond
     ((pair? x) (append (loop (car x)) (loop (cdr x))))
     (else (list x))))))
 (λ (x y)
  (let ((xnr (type-nr x)) (ynr (type-nr y)))
   (or (< xnr ynr)
    (and (= xnr ynr)
     (cond
      ((char? x) (char<? x y))
      ((number? x) (< x y))
      ((string? x) (string<? x y))
      ((symbol? x) (symbol<? x y))
     (else (error 'all-atoms "~s ~s" x y)))))))))
