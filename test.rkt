#lang racket

(require "simplisp.rkt" #;simplispbc/simplisp test/test)

(define simple simplisp)
;(define simple (simplisp source-code))
;(test 0 ((print (parameterize ((current-output-port (open-output-nowhere)))
;                 (simplisp '(trace-option #t) source-code)))) #f #:output "#<procedure:simplisp>")

(define (xprint x) (printf "~s " x))

(test 1 ((simple '(reset) '(let ((a 1)) (list (set! a 2) a)))) '((1 2)))
(test 2 ((simple '((lambda (x y z) (z x y)) 2 3 +))) '(5))

(test 3
 ((simple '((make-macro 'my-macro (λ (uargs env) (printf "~s~n~s~n" uargs env))) aap noot mies)))
 #f #:output "(aap noot mies) #<empty-env>")

(test 4 ((simple '(let loop ((n 5)) (if (zero? n) 1 (* n (loop (sub1 n))))))) '(120))
(test 5 ((simple '(let loop ((n 5) (r 1)) (if (zero? n) r (loop (sub1 n) (* n r)))))) '(120))

(test 6
 ((simple `(let ((p (make-promise 'pp 'lazy (λ () (,xprint 'ppp) 'pppp))))
               (,xprint (promise-state p))
               (,xprint (force p))
               (,xprint (promise-state p))
               (,xprint (force p))
               (,xprint p))))
 #f #:output "lazy ppp pppp forced pppp #<promise:pp:forced>")

(test 7
 ((define-values (make-p p? p-delay p-lazy p-force) (simple '(make-promise-type 'P)))
  (simple `(let ((p (,make-p 'pp 'lazy (λ () (,xprint 'ppp) 'pppp))))
               (,xprint (promise-state p))
               (,xprint (force p))
               (,xprint (,p-force p))
               (,xprint (promise-state p))
               (,xprint (force p))
               (,xprint (,p-force p))
               (,xprint p)
               (,xprint (force p)))))
 #f #:output "lazy #<P:pp:lazy> ppp pppp forced #<P:pp:forced> pppp #<P:pp:forced> #<P:pp:forced>")

(test 8
 ((simple '(let* ((a 1) (b (add1 a)) (a (add1 b))) (list a b))))
 '((3 2)))

(test 9
 ((simple '(letrec ((! (λ (n) (if (zero? n) 1 (* n (fact (sub1 n)))))) (fact !)) (fact 5))))
 '(120))

(test 10
 ((simple
  '(letrec ((! (λ (n) (fact n 1)))
            (fact (λ (n m) (if (zero? n) m (fact (sub1 n) (* n m))))))
           (! 5)))) '(120))

(test 11
 ((simple
  '(letrec
    ((str-cdr (λ (str) (force (cdr str))))
     (make-str (λ (next n) (cons n (lazy (make-str next (next n))))))
     (ints (make-str add1 0))
     (frontier
      (λ (str n)
       (if (zero? n) '()
        (cons (car str) (frontier (str-cdr str) (sub1 n))))))
     (Q
      (λ (str n)
       (if (zero? (remainder (car str) n))
        (Q (str-cdr str) n)
        (cons (car str) (lazy (Q (str-cdr str) n))))))
     (P
      (λ (str)
       (cons (car str) (lazy (P (Q str (car str))))))))
    (frontier (P (str-cdr (str-cdr ints))) 10))))
 '((2 3 5 7 11 13 17 19 23 29)))

(test 12
 ((simple '(force (lazy (print (call-with-values (λ () (make-promise-type 'P)) list))))))
 #f
 #:output
 "(#<procedure:make-P> #<procedure:P?> #<macro:P:delay> #<macro:P:lazy> #<procedure:force-P>)")

(test 13
 ((simple
  '(let-values (((a b c) (values 1 2 3)) ((b c d) (values 4 5 6)) ((c d e) (values 7 8 9)))
    (list a b c d e)))) '((1 4 7 8 9)))

(test 14
 ((simple
  '(let*-values (((a b c) (values 1 2 3)) ((a b c) (values (add1 c) (add1 b) (add1 a))) ((e) 4))
    (list a b c e)))) '((4 3 2 4)))

(test 15
 ((simple 
  '(letrec-values
    (((even odd)
      (values
       (λ (n) (if (zero? n) #t (odd (sub1 n))))
       (λ (n) (if (zero? n) #f  (even (sub1 n)))))))
    (list (map even '(0 1 2 3 4 5)) (map odd '(0 1 2 3 4 5))))))
 '(((#t #f #t #f #t #f) (#f #t #f #t #f #t))))

(test 16
 ((equal?
   (simplisp
   '(racket
    '(simplisp
     '(racket
      '(simplisp
       '(racket
        '(simplisp 'special-vars)))))))
   (simplisp 'special-vars))) '(#t))

(test 17 ((simple '(let ((a 3) (b 4) (+ *)) (eval '(+ a b) (current-env))))) '(12))

(test 18
 ((simple '(let ((a 3) (b 4)) (with-env (current-env) (list a b))))) '((3 4)))

(test 19
 ((simple '(let ((add1 3) (sub1 4)) (with-env #f (list add1 sub1))))) (list (list add1 sub1)))

(test 20
 ((simple
  '(let ((add1 3) (sub1 4))
    (list
     (with-env 'copy (set! add1 40) (set! sub1 50) (list add1 sub1))
     (list add1 sub1))))) '(((40 50) (3 4))))

(test 21
 ((simplisp
 '(letrec
   ((stream-map
     (λ (proc a b)
       (stream-cons
        (proc (stream-car a) (stream-car b))
        (stream-map proc (stream-cdr a) (stream-cdr b)))))
    (stream-take
     (λ (strm n)
      (if (zero? n) '()
       (cons (stream-car strm) (stream-take (stream-cdr strm) (sub1 n))))))
    (plus
     (λ (x y) (printf "~s ~s~n" x y) (+ x y)))
    (stream-ref
     (λ (strm n)
      (if (zero? n) (stream-car strm)
       (stream-ref (stream-cdr strm) (sub1 n)))))
    (fibonacci
     (λ (n m)
      (stream-cons n (fibonacci m (plus n m))))))
   (stream-ref (fibonacci 0 1) 10))))
 '(55)
 #:output
 "0 1
  1 1
  1 2
  2 3
  3 5
  5 8
  8 13
  13 21
  21 34
  34 55")

(test 22
 ((simplisp
  '(catch-exn (λ (exn) (printf "~a~n" (exn-message exn)) 'catched)
    (printf "aap~n") (error "noot") this is ignored)))
 '(catched)
 #:output "aap noot")

(test 23
 ((simplisp
  '(global-define a 1)
  '(global-define-values (b c d) (values 2 3 4))
  '(printf "~s~n" (global-ref c))
  '(global-ref-values a b d)))
 '(1 2 4) #:output "3")

(test 24
 ((simplisp
  '(global-define a 1)
  '(global-define-values (b c d) (values 2 3 4))
  '(printf "~s" (call-with-values (λ () (global-set!-values (a b c d) (values 10 20 30 40))) list))
  '(global-ref-values a b c d)))
 '(10 20 30 40) #:output "(1 2 3 4)")

(test-report)