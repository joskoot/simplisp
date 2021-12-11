#lang scribble/manual

@;====================================================================================================

@(require
  scribble/core
  scribble/eval   
  racket
  "simplisp.rkt"
  "scribble-utensils.rkt"
  (for-label "simplisp.rkt" racket)
  (for-template "simplisp.rkt" racket)
  (for-syntax racket)) 

@(define nr-of-vars 0)
@(define (github?) #f)

@title[#:version ""]{Simplisp, @nb{a toy interpreter}}
@author{Jacob J. A. Koot}
@(defmodule simplispbc/simplisp #:packages ())
@;@(defmodule "simplisp.rkt" #:packages ())

@note{@red{WARNING}@(lb)
Includes trace options, but with Racket CS the options@(lb)
@(hspace 5)@nbpr{trace-value}, @nbpr{trace-assgn} and @nbpr{trace-varef}@(lb)
cannot be used when simplisp traces its own source code.@(lb)
Works well with Racket BC and Racket versions up to and including 7.9.@(lb)
Problems with parameterization with Racket CS 8.0 and up.}

@section[#:tag "1"]{Introduction}
@;Module @hyperlink["simplisp.rkt"]{simplisp.rkt} provides:
Module @hyperlink["../../simplisp.rkt"]{simplisp.rkt} provides:

@inset{@Tabular[
(("procedure"            @nbr[simplisp])
 ("symbolic expression"  @nbr[source-code]))
#:sep (hspace 3)]}

Simplisp is a meta-recursive interpreter.
Its source-code consists of one single symbolic expression
in a proper intersection of the implemented language and
@nbhl["file:///C:/Program%20Files/Racket/doc/reference/index.html"]{@tt{racket/base}}.
Simplisp does no compilation and has no expansion phase prior to the evaluation proper.
Macro calls are interpreted on the fly during evaluation.
Macros are first class run-time data. 
They can be passed around in run-time variables and new ones can be created at run-time,
just like procedures.
Simplisp is eager.
Procedures are called by value.
@nb{In binding} forms, such as @nber["let"]{let},
the expressions for the values to be bound are evaluated before the body of the form is evaluated.
For lazy evaluation simplisp provides @nber["make-promise-type"]{promise types} and
@nber["make-stream-type"]{stream types}. 
When @nber["trace-option"]{tracing} is not enabled, tail positions are handled correctly.
More details in section @nbsr["2"].

@Elemtag{simplisp}
@defproc[
(simplisp (top-expr #,(black @nbr[any/c] " evaluating to " @nbr[any])) ... ) any]{
Because procedure @nbr[simplisp] is called by value,
it receives the values of its arguments.
@nb{We call} these values `top-expressions'.
@nbrl[simplisp]{Simplisp} evaluates the top-expressions from left to right and
returns the value or multiple value of the last one.
When called without arguments, procedure @nbr[simplisp] immediately returns @(Void).
The procedure is available as a @seclink["7"]{predefined variable} within @nbr[simplisp] itself too.}

@elemtag{source-code}
@defidform[#:kind "symbolic expression" source-code]{
Variable containing the @nbhl["simplisp.rkt"]{source code} of procedure @nbr[simplisp].
The code can be evaluated @nb{by @(Rckt)} and by @nbr[simplisp] itself,
yielding equivalent instances of @nbr[simplisp],
although some of them can be much slower than others.
@nb{See section @secref["5"].}}

@section[#:tag "2"]{Simplisp ≠ Racket}

@nbrl[simplisp]{Simplisp} differs very much from @(Rckt).
It is much smaller and much simpler. @nb{It is a toy}.

@itemlist[#:style 'ordered
@item{Macros are first class run-time data, just like procedures.

@Interaction[
(simplisp '(values lambda (macro? lambda)))]
@Interaction[
(simplisp
'(let*
  ((list-shift-circular-right
    (lambda (lst)
     (let ((lst (reverse lst)))
      (cons (car lst) (reverse (cdr lst))))))
   (postfix
    (macro (x) (list-shift-circular-right x))))
  (values postfix (macro? postfix) (postfix (1 2 3 +)))))]
@Interaction[
(simplisp
'(let* ((function lambda) (2j+1 (function (j) (add1 (* 2 j)))))
  (values
   function
   2j+1
   (and (closure? 2j+1) (procedure? 2j+1)) (code:comment #,(black "See " @nbpr{closure?}))
   (2j+1 3/2))))]}

@item{@elemtag{environment}
An environment maps symbols to values and has two layers:

@inset{• local environment@(lb)
       • clean environment}

A variable reference is evaluated by first looking in the local environment.
If not found there, it is looked for in the clean environment.
If not found there either, an exception is raised.
For every @nber["simplisp"]{top-expression} the evaluation starts with empty local environment.
@nb{The clean} environment is never mutated and contains all @seclink["7"]{predefined variables},
those containing macros included.
Environments are first class data, in fact procedures.
@nb{The current} environment can be retrieved with macro @nbpr{current-env}.
Procedure @nbpr{eval} and macro @nbpr{with-env} allow switching between environments. 
The body of a procedure is evaluated in the environment
in which the procedure was created and to which local bindings are added for the arguments,
possibly shadowing other bindings.
In principle, a procedure is insensitive to local bindings of the calling site.

@Interaction[
(simplisp
'(let* ((a 1) (b 2) (f (λ () (list a b))))
  (let ((a 3) (b 4))
   (values (f) (list a b)))))]

It is possible, though, to make a procedure sensitive to the environment from where it
is called by taking the environment as an argument:

@Interaction[
(simplisp
'(let ((f (λ (x e) (e x))))
  (values
   (let ((a 1)) (f 'a (current-env)))
   (let ((a 2)) (f 'a (current-env))))))]}

@item{Given an expression or subexpression consisting of an immutable non-empty list,
@nbr[simplisp] evaluates the first element,
which must yield an operator, id est,
a procedure, a macro or the empty list.
Accordingly, the expression or subexpression is a procedure call,
a macro call or a @nbpr{null-form}.

@itemlist[@item{@bold{Procedure call}@(lb)
The arguments in a procedure call are evaluated from left to right
and the values are passed to the procedure.
An argument may produce a multiple value.
The elements of @nb{a multiple} value are passed to the procedure as separate arguments.
This holds for the arguments of procedure @nbr[values] too.
For example:

@Interaction[
(simplisp
'(list (quotient/remainder 10 3)))]

does the same as:

@Interaction[
(simplisp
'(call-with-values (λ () (quotient/remainder 10 3)) list))]

As another example:

@Interaction[
(simplisp
'(+
  (values 1 2)
  3
  (values)
  (values (quotient/remainder (+ (* 4 6) 5) 6)
          (values 6 7))))]

yields the same as:

@Interaction[
(simplisp '(+ 1 2 3 4 5 6 7))]}

@item{@bold{Macro call}@(lb)
Whether or not, when, how and in which order arguments are evaluated in case of a macro call,
depends on the macro and its arguments.
New macros can be made at run time with procedure @nbpr{make-macro} and macro @nbpr{macro}.
Macros are first class data and can be passed around, just like procedures:

@Interaction[
(simplisp '(map (λ (op) (op 1 2)) (list or and)))
(simplisp '(map (λ (b) ((if b or and) 1 2)) '(#t #f)))]}

@item{@bold{Null-form}@(lb)
For recollection of a value from the clean environment, bypassing the local environment.
See the @nber["null-form"]{related documentation}.}]}

@item{Everything else than a symbol or an immutable proper list is self-evaluating,
the empty list included.

@Interaction[
(simplisp (cons 1 2))]

@Interaction[
(simplisp add1)]

@Interaction[
(simplisp (λ (x) x))]

@Interaction[
(simplisp '(simplisp (1 . 2)))]

@Interaction[
(simplisp '(simplisp #:keyword))]

@Interaction[
(simplisp '(simplisp (λ ())))]

@Interaction[
(simplisp '(simplisp lambda))]

@Interaction[
(simplisp '())]

@Interaction[
(simplisp '(simplisp ()))]

In the following example, @nbr[simplisp] receives the list
@(nb (tt (~a (eval '(list add1 2) (make-base-namespace))))).@(lb)
Both elements of this list are self-evaluating.

@Interaction[
(simplisp '(trace-option 'all))
(simplisp (list add1 2))]
Compare this with:
@Interaction[
(simplisp '(trace-option 'all))
(simplisp '(add1 2))]}

@item{@elemtag{global table}All forms are expressions.
@nbrl[simplisp]{Simplisp} has one @seclink["expand-context-model"
#:doc '(lib "scribblings/reference/reference.scrbl")]{evaluation context} only.
It has no @nbr[define]- nor @nbr[define-values]-forms.
In stead it has parameter
@nbpr{current-global-table} containing a mutable table that maps symbols onto values.
Initially the parameter contains an empty table.
It can be used as a kind of top-environment without disturbing the
@elemref["environment"]{local or clean environment}.
@nb{A variable} reference never uses the @nbpr{current-global-table}.
The table can be accessed by means of macros
@nbpr{global-ref}, @nbpr{global-set!}, @nbpr{global-define},
@nbpr{global-ref-values}, @nbpr{global-set!-values} and @nbpr{global-define-values} only.
The parameter is part of the @seclink["4"]{internal state} of @nbr[simplisp].
Because it is preserved between successive calls to @nbr[simplisp],
values can be stored for use in subsequent @nbrl[simplisp]{top-expressions}
and in subsequent calls to procedure @nbr[simplisp].

@Interaction[
(simplisp '(global-define flower 'forget-me-not))
(simplisp '(global-ref flower))]}

@item{In all forms having a body, the latter may be empty, in which case the value is @(Void).
The last element of a non-empty body is evaluated in tail position of the body.

@Interaction[
(write (simplisp '((λ ()))))
(write (simplisp '(case 'key ((key)) (else 'ignored))))
(write (simplisp '(let ())))
(write (simplisp '(begin)))]}

@item{In binding forms, the names of the variables are not required to be distinct.
Within the same binding form, a variable shadows all variables of the same name at its left.

@Interaction[
(simplisp '((λ (a b a) (list a b)) 1 2 3))
(simplisp '(let ((a 1) (b 2) (a 3)) (list a b)))
(simplisp '(let-values (((a b a) (values 1 2 3))) (list a b)))]

A subexpression whose value is to be bound to a shadowed variable is evaluated too:

@Interaction[
(simplisp '((λ (a a) a) (displayln "evaluated") 2))
(simplisp '(let ((a (displayln "evaluated")) (a 2)) a))]}

@item{In @nbr[simplisp], procedures have no keyword arguments nor optional arguments.@(lb)
For @nbr[simplisp], keywords are regular self-evaluating arguments.

@Interaction[
(simplisp '(list #:x 1 #:y 2))]

@Interaction[
(define (f #:a (a 'omitted) . b) (cons a b))
(code:line (f #:a 3 4 5) (code:comment #,(green "Called from Racket:")))
(code:comment #,(list (green "Ok, ") (red "but called  from ") (red (tt "simplisp")) (black ":")))
(simplisp `(,f #:a 3 4 5))]

Within @nbr[simplisp] it is a bad idea to use keyword arguments in a call to
a procedure borrowed from or defined within @(Rckt).
A procedure with required keyword arguments cannot be called from @nbr[simplisp].
Optional keyword arguments must be omitted.}

@item{The booleans are @nbr[#f] for false and @nbr[#t] for true,
but everything else than @nbr[#f] is accepted as true.
Hence @nbr[#f] is the only and unique false value.
Some parameters, such as @nbr[print-graph] and @nbpr{trace-start},
coerce an arbitrary true value to @nbr[#t],
for example:

@Interaction[
(simplisp
'(trace-start ()) (code:comment #,(black "The empty list is a true value."))
'(trace-start))]
The same happens for some parameters of Racket:
@Interaction[
(parameterize ((print-graph '(this is true))) (print-graph))]}]

@section[#:tag "3"]{Data types}
@nber["lambda"]{Closures},
@nber["make-macro"]{macros},
@nber["make-promise-type"]{promises},
@nber["make-stream-type"]{streams},
@nber["environment"]{environments} and
@nber["global table"]{global tables}
are data types specific for @nbr[simplisp], in fact @nbr[struct] types.
@nber["lambda"]{Closures} and @nber["environment"]{environments}
have procedure property, both in @nbr[simplisp] and in @(Rckt).
@(elemtag "data-exchange")
Data can be exchanged between @nbr[simplisp] and @(Rckt),
including procedures, lists, vectors, characters, strings, boxes, sets, hash-tables,
multiple values and much more.

Exchange of a box between @(nbr simplisp) and @(Rckt):

@Interaction[
(unbox (simplisp '(box "to be unboxed")))
(simplisp `(unbox ,(box "to be unboxed")))]

Calling a procedure made by @(nbr simplisp) from @(Rckt):

@Interaction[
(define proc (simplisp '(λ (x) (add1 (* 2 x)))))
(proc 3)]

A procedure made by @(nbr simplisp) is insensitive to bindings in @(Rckt):

@Interaction[
(let ((* /) (values list))
 (define add1 sub1)
 (printf " ~nComputed by Racket:~n")
 (printf "~s~n" (values * add1))
 (printf " ~nComputed by simplisp:~n") 
 (simplisp '(values * add1)))]

Exchange of a continuation from @nbr[simplisp] to @(Rckt):

@Interaction[
(let ((cc #f) (n (make-parameter 3)))
 (set! cc
  (simplisp
  `(let ((cc #f))
    (let/cc x (set! cc x))
    (println (,n))
    cc)))
 (unless (zero? (n)) (n (sub1 (n))) (cc)))]

Exchange of a continuation from @(Rckt) to @nbr[simplisp]:

@Interaction[
(let ((cc #f))
 (simplisp '(global-define n (make-parameter 3)))
 (let/cc x (set! cc x))
 (simplisp
 `(let ((n (global-ref n)))
   (unless (zero? (n))
    (println (n))
    (n (sub1 (n)))
    (,cc)))))]

Use of @nbr[simplisp]'s procedure @nbpr{eval}, parameter @nbpr{trace-option}@(lb)
and an @nber["environment"]{environment} in @(Rckt):

@Interaction[
(define-values (evaluator trace environment)
 (simplisp
 '(values
   eval
   trace-option
   (let ((a 3) (b 5)) (current-env)))))
(code:line (environment 'a 8) (code:comment #,(black "Assigns 8 and returns old value 3.")))
(code:line (environment 'a)   (code:comment #,(black "Returns the assigned value 8.")))
(parameterize ((trace 'all)) (evaluator '(+ a (* b 2)) environment))]

Exchange of multiple value between @nbr[simplisp] and @(Rckt):

@Interaction[
(call-with-values
 (λ () (values 1 2 3))
 (simplisp '(λ x (apply + x))))]

@Interaction[
(call-with-values
 (simplisp '(λ () (values 1 2 3)))
 (λ x (apply + x)))]

Using a simplisp environment in Racket:

@Interaction[
(define env (simplisp '(let ((a 1) (b 2)) (current-env))))
(map env '(a b lambda))]

@section[#:tag "4"]{Internal state}

@nbrl[simplisp]{Simplisp} has internal state of which the following elements are
preserved between successive calls to @nbr[simplisp]:

@inset{@Tabular[(("parameter" @nbpr{trace-option})
                 ("parameter" @nbpr{trace-align})
                 ("parameter" @nbpr{trace-start})
                 ("parameter" @nbpr{trace-finis})
                 ("parameter" @nbpr{trace-value})
                 ("parameter" @nbpr{trace-varef})
                 ("parameter" @nbpr{trace-assgn})
                 ("parameter" @nbpr{trace-selfi})
                 ("parameter" @nbpr{current-global-table})
                 (
@seclink["namespace-model" #:doc '(lib "scribblings/reference/reference.scrbl")]{namespace}
(list "used by procedure " @nbpr{racket}))) #:sep (hspace 1)]}

The initial internal state can be reëstablished by means of procedure @nbpr{reset}.@(lb)
The @elemref["environment"]{clean environment}
is never mutated and always is accessible with the @nbpr{null-form}.

@section[#:tag "5"]{Distinct instances}

There are several ways to produce more instances of procedure @nbr[simplisp].@(lb)
Each instance has its own @seclink["4"]{internal state}.

By means of @nbr[simplisp] itself:

@Interaction[(simplisp source-code)]

By means of @(Rckt)'s procedure @nbr[eval]:

@Interaction[(eval source-code (make-base-namespace))]

By means of procedure @nbpr{eval} of @nbr[simplisp]:

@Interaction[(simplisp `(eval ',source-code))]

By requiring it in a fresh namespace:

@Interaction[
(parameterize ((current-namespace (make-base-namespace)))
 (dynamic-require "simplisp.rkt" 'simplisp))]

Distinct instances of @nbr[simplisp] recognize most,
but not all of each other's data.
@nb{For example,} distinct instances of @nbr[simplisp] do not recognize each other's
@nber["closure?"]{closures}.
@nb{They recognize} each other's closures as procedures, though:

@Interaction[
(define simplisp1 (simplisp source-code))
(define closure (simplisp '(λ ())))
(simplisp  `(list (procedure? ,closure) (closure? ,closure)))
(simplisp1 `(list (procedure? ,closure) (closure? ,closure)))]

Distinct instances of @nbr[simplisp] do not recognize each other's macros:

@Interaction[
(define simplisp1 (simplisp source-code))
(define λ (simplisp 'λ))
(values
 (simplisp  `(macro? ,λ))
 (simplisp1 `(macro? ,λ)))]

Every instance has its own parameters @nbpr{trace-option}, @nbpr{trace-width}, @nbpr{trace-align},
@nbpr{trace-start}, @nbpr{trace-finis}, @nbpr{trace-value}, @nbpr{trace-varef}, @nbpr{trace-assgn},
@nbpr{trace-assgn} and @nbpr{current-global-table}, for example:

@Interaction[
(define simplisp1 (simplisp source-code))
(simplisp '(trace-width 100))
(values
 (simplisp  '(trace-width))
 (simplisp1 '(trace-width)))]

@section[#:tag "6"]{Inferred names}
The data types mentioned in @seclink["3"]{section 3}
have no symbolic printed form, but they can have a name.
The printed form is @tt{#<@italic{type}:@italic{name}>} where @tt{@italic{name}}
is a symbol for named objects and @nbr[#f] for objects without name. 
When an object is bound to or assigned to a variable and has no name yet,
it is named after the variable.
For some data types the printed form provides additional information.
@nb{The inferred} name can be retrieved with procedure @nbr[object-name].

@Interaction[(simplisp '(λ ()))]

@Interaction[(simplisp '(let ((c (λ ()))) (values c (object-name c))))]

@Interaction[(simplisp '(let ((f #f)) (set! f (λ ())) f))]

@Interaction[(simplisp '(global-define (g)))]

@Interaction[(simplisp '(global-define m (macro ())))]

@Interaction[
(simplisp
'(letrec ((p (lazy (writeln p))))
  (writeln p)
  (force p)
  (writeln p)))]

@section[#:tag "7"]{Predefined variables}
@(define Special-vars (simplisp '(special-vars)))
@(define nr-of-special-vars (length Special-vars))
@(define nr-of-borrowed-vars (length (simplisp '(borrowed-vars))))
@(define nr-of-clean-vars (length (simplisp '(clean-vars))))
@(unless (= (+ nr-of-borrowed-vars nr-of-special-vars) nr-of-clean-vars)
  (error 'variable-counts "~s ~s ~s" nr-of-special-vars nr-of-borrowed-vars nr-of-clean-vars))

The @elemref["environment"]{clean environment} contains
@(roman (number->string nr-of-clean-vars)) variables of which
@(roman (number->string (- nr-of-clean-vars nr-of-special-vars)))
contain objects directly borrowed from @(Rckt) and
@(roman (number->string nr-of-special-vars)) are
special variables with objects provided by @nbr[simplisp].
Procedure @nbpr{clean-vars}
returns a sorted list of the names of all clean variables.
Procedure @nbpr{borrowed-vars}
returns a sorted list of the names of all borrowed variables.
These lists are too long to present here.
@elemtag{simplisp-variables}
Procedure @nbpr{special-vars} returns a sorted list of the names of the
@(roman (number->string nr-of-special-vars))
special variables. 
These are:

Special @nbr[simplisp] procedures, parameters excluded:

@(inset
  (let
   ((lst
     (for*/list ((sv (in-list Special-vars))
                 #:when
                 (let ((val (get-simplisp-val sv)))
                  (and (procedure? val) (not (parameter? val)))))
      (list (nbpr (symbol->string sv)) (hspace 1) " "))))
   (set! nr-of-vars (+ nr-of-vars (length lst)))
   lst))

Special @nbr[simplisp] parameters:

@(inset
  (let
   ((lst
     (for*/list ((sv (in-list Special-vars)) #:when (parameter? (get-simplisp-val sv)))
      (list (nbpr (symbol->string sv)) (hspace 1) " "))))
   (set! nr-of-vars (+ nr-of-vars (length lst)))
   lst))

All predefined macros are special. These are:

@(inset
  (let ((macro? (simplisp 'macro?)))
   (let
    ((lst
      (for*/list ((sv (in-list Special-vars)) #:when (macro? (get-simplisp-val sv)))
       (list (nbpr (symbol->string sv)) (hspace 1) " "))))
    (set! nr-of-vars (+ nr-of-vars (length lst)))
    lst)))

Other special @nbr[simplisp] objects:

@(inset
  (let ((macro? (simplisp 'macro?)))
   (let
    ((lst
      (for*/list ((sv (in-list Special-vars))
               #:when (let ((val (get-simplisp-val sv)))
                        (and (not (or (macro? val) (procedure? val))))))
       (list (nbpr (symbol->string sv)) (hspace 1) " "))))
    (set! nr-of-vars (+ nr-of-vars (length lst)))
    lst)))

@(when (not (= nr-of-vars (length Special-vars)))
       (error "predefined vars"))

The remainder of this section provides descriptions of all special @nbr[simplisp] objects
in alphabetical order of their names, with exception of procedure @nbr[simplisp],
which already has been described @nbrl[simplisp]{above}.

@ignore{@nbrl[simplisp]{Simplisp} uses a
@seclink["namespace-model"
#:doc '(lib "scribblings/reference/reference.scrbl")]{base namespace}
for the clean environment, which contains all objects borrowed from @(Rckt) and to which the special
variables are added or replace @(Rckt)-variables.
The base-namespace may contain objects which are of no use within @nbrl[simplisp]{simplisp}.
For example, a @(Rckt) procedure with a required keyword argument cannot be called within
@nbrl[simplisp]{simplisp} and requires an @nber["racket"]{escape} to @(Rckt).
With DrRacket version 7.5 [3m], the clean namespace has no procedures
with required keyword arguments, but this may change in future versions, who knows?.}

@Elemtag{and}
@defform*[#:kind "macro" ((and) (and expr ... last-expr))
          #:contracts ((expr any/c) (last-expr any))]{
When called without arguments, the macro returns @nbr[#t].
Otherwise,
the @tt{@italic{exprs}} are evaluated until and including the first one that yields @nbr[#f],
in which case @nbr[#f] is returned and the remaining @tt{@italic{exprs}} and the @nbr[last-expr]
are not evaluated. If none of the @tt{@italic{exprs}} yields @nbr[#f],
the @nbr[last-expr] is evaluated in tail position and its value or multiple value is returned.}

@Elemtag{begin}
@defmacro[(begin expr ...)]{
The @tt{@italic{exprs}} are evaluated from left to right ignoring their values
except the value or multiple value of the last @nbr[expr], which is returned.
If no @nbr[expr] is present the result is
@(Void).
The last @nbr[expr] is evaluated in tail position.
Hence, the last @nbr[expr] of a nested begin-form is evaluated in tail position too.}

@Elemtag{bindings}
@defmacro[(bindings var) #:contracts ((var symbol?))]{
The argument is evaluated. The macro returns a list of zero, one, two or three
of the symbols @nbr['local], @nbr['clean] and @nbr['global] (in this order)
showing whether or not and where a variable of the name @nbr[var] is bound.
If @nbr[var] has no bindings, the returned list is empty.

@Interaction[
(simplisp
'(let ((a 1) (add1 2))
  (global-define add1 3)
  (for-each
   (λ (var) (printf "bindings of ~s: ~s~n" var (bindings var)))
  '(a simplisp add1 unbound))))]

It always is possible to select a specific binding of a variable:

@Interaction[
(simplisp
'(let ((add1 'local-add1))
  (global-define add1 'global-add1)
  (printf "Clean  binding: ~s~n" (() add1))
  (printf "Local  binding: ~s~n" add1)
  (printf "Global binding: ~s~n" (global-ref add1))))]}

@Elemtag{borrowed-vars}
@defproc[(borrowed-vars) (listof symbol?)]{
Returns a sorted list of variables containing objects directly borrowed from @(Rckt).}

@Elemtag{case}
@defmacro[(case key-expr clause ... maybe-else-clause)
          #:grammar ((clause ((datum ...) body))
                     (maybe-else-clause (code:line)
                                        (#,(tt "else") body))
                     (body (code:line expr ...)))]{
Like @nbr[case] of @(Rckt) but allowing a @nbr[body] to be empty, which yields @(Void).
@(tt "else") is a plain symbol. For macro @nbpr{case} its value is irrelevant
and it does not even need a binding.}

@Elemtag{catch-exn}
@defmacro[(catch-exn catcher expr ...)
#:contracts ((catcher (-> exn:fail? any)))]{
Same as
@inset{@tt{(@nbpr{with-handlers*} @nbr[((exn:fail? catcher))] @nbr[expr] ...)}}
First @nbr[catcher] is evaluated.
Subsequently the @nbr[expr]s are evaluated as in a @nbpr{begin}-form.
However, when an exception is raised satisfying predicate @nbr[exn:fail?],
the @nbr[catcher] is called with the exception for its argument.
The @nbr[catcher] can finish by reraising the exception or by returning normally,
in which case the returned value or values are returned
as the value or values of the whole @nbpr{catch-exn}-form.

@Interaction[
(simplisp
'(catch-exn
  (λ (exn) no error: hence we do not get here)
  (displayln "1 2 3") (values 4 5 6)))]

@Interaction[
(simplisp
'(catch-exn
  (λ (exn) (eprintf "~a~n" (exn-message exn))
           (values 'hash 'been 'catched))
  (displayln "1 2 3") (error "monkey")
  this will be ignored))]

@Interaction[
(simplisp
'(catch-exn
  (λ (exn)
   (displayln "exception catched")
   (raise exn))
  (error "and raised again")
  this will be ignored))]}

@Elemtag{clean-vars}
@defproc[(clean-vars) (listof symbol?)]{
Returns a sorted list of the names of all clean variables.}

@Elemtag{closure?}
@defproc[#:kind "predicate" (closure? (obj any/c)) boolean?]{
Predicate for closures.
These are procedures made by macro @nbpr{lambda} or @nbpr{λ}.

@Interaction[
(simplisp
'(map (λ (x) (list (procedure? x) (closure? x))) (list add1 (λ ()))))]}

@Elemtag{cond}
@defmacro[(cond (test expr ...) ...)
#:contracts
((test any/c)
 (expr any))]{
The @nbr[test]s are evaluated until one is found yielding a true value.
The related clause is selected and the remaining ones are ignored.
If none of the tests yields a true value, the @nbpr{cond}-form yields
@(Void).
If the selected clause has no @nbr[expr]s, the value of the @nbr[test] is returned,
else the @nbr[expr]s are evaluated from left to right and the value or multiple value
of the last one is returned.}

@Elemtag{copy-env}
@defproc[(copy-env (env #,(nbpr "env?"))) #,(nbpr "env?")]{
Returns a copy of an environment as returned by macro @nbpr{current-env} or procedure
@nbpr{empty-env}.
Assignments in the copy are not effective in the original and vice versa.}

@Elemtag{copy-global-table}
@defproc[(copy-global-table (table #,(nbpr "global-table?"))) #,(nbpr "global-table?")]{
Copies a @elemref["global table"]{global table}.
Mutations of the copy by means of macros
@nbpr{global-define}, @nbpr{global-set!}, @nbpr{global-define-values}
and @nbpr{global-set!-values}
do not affect the original and vice-versa.}

@Elemtag{current-env}
@defproc*[#:kind "macro yielding a procedure" (((current-env) env?)
                     (((current-env) (var symbol?)) any/c)
                     (((current-env) (var symbol?) (value any/c)) any/c))]{
@nbpr{current-env} is a macro.
It returns the current environment,
which is a procedure that can be used as shown in the second and third form.
The second form is a variable reference.
@nb{It retrieves} the value of @nbr[var]
or raises an exception if the @nbr[var] is not bound.
The third form assigns the @nbr[value] to the @nbr[var], which already must have a local binding.
It is like an assignment with @nbr[set!], but the @nbr[var] is evaluated
and the @nbr[value] is assigned to the variable named by the resulting symbol.
It returns the original value of the variable.
An exception is raised if the variable has no local binding.

@Interaction[
(simplisp
'(let* ((a 1) (e (current-env)))
  (list (e 'a) (e 'a 2) (e 'a))))]

@Interaction[
(simplisp '((current-env) 'current-env))]

It is not possible to alter a clean variable.

@Interaction[
(simplisp '((current-env) 'list vector))]}

@Elemtag{current-global-table}
@defparam[current-global-table table #,(nbpr "global-table?")]{
This parameter is part of the internal state of @nbr[simplisp].
It contains a table that is used by macros
@elemref["global-ref"]{global-ref},
@elemref["global-set!"]{global-set!},
@elemref["global-define"]{global-define},
@elemref["global-ref-values"]{global-ref-values},
@elemref["global-set!-values"]{global-set!-values} and
@elemref["global-define-values"]{global-define-values}. 
The table can be used as a mutable top environment that is preserved between successive
calls to @nbr[simplisp].

@Interaction[
(simplisp '(global-define n 10))
(simplisp '(global-ref n))]

The @nbpr{current-global-table} is a parameter and can contain other global-tables,
which makes it possible to switch between global tables, for example:

@Interaction*[
(simplisp
 '(global-define global-table-one (empty-global-table)))]
@Interaction*[
(simplisp
 '(global-define global-table-two (empty-global-table)))]
@Interaction*[
(simplisp
 '(parameterize* ((current-global-table (global-ref global-table-one)))
   (global-define a '1-one) (global-define b '2-one) (void)))]
@Interaction*[
(simplisp
 '(parameterize* ((current-global-table (global-ref global-table-two)))
   (global-define a '1-two) (global-define b '2-two) (void)))]
@Interaction*[
(simplisp
 '(parameterize* ((current-global-table (global-ref global-table-one)))
   (list (global-ref a) (global-ref b))))]
@Interaction*[
(simplisp
 '(parameterize* ((current-global-table (global-ref global-table-two)))
   (list (global-ref a) (global-ref b))))]
@(reset-Interaction*)}

@Elemtag{delay}
@defmacro[(delay expr ...)]{
Same as
@tt["(" @nbpr{make-promise} " " @nbr[#f] " " @nbr['delay]  " (" @nbpr{λ} " () " @nbr[expr] " ...))"]}

@Elemtag{else}
@defthing[#:kind "value" else symbol? #:value 'else]{
Can be used for the @tt{@italic{else-clause}} in a @nbpr{cond}-form
(provided it is not rebound to @nbr[#f]).}

@Elemtag{empty-env}
@defthing[#:kind "value" empty-env #,(nbpr "env?")]{
Variable containing the unique @elemref["environment"]{environment} with empty local layer.
It includes the clean environment, though.
@Interaction[
(simplisp '(eq? (current-env) empty-env))]}

@Elemtag{empty-env?}
@defproc[(empty-env? (obj any/c)) boolean?]{
Returns @nbr[#t] if the @nbr[obj] is the unique @elemref["environment"]{environment}
with empty local layer, else returns @nbr[#f].}

@Elemtag{empty-global-table}
@defproc[(empty-global-table) #,(nbpr "global-table?")]{
Returns an empty @elemref["global table"]{global table}.}

@Elemtag{env?}
@defproc[#:kind "predicate" (env? (x any/c)) boolean?]{
Returns @nbr[#t] if @nbr[x] is an environment as returned by macro @nbpr{current-env},
else returns @nbr[#f].@(lb)
Also yields @nbr[#t] for @nbpr{empty-env}.}

@Elemtag{eval}
@defproc[(eval (expr any/c) (env #,(nbpr "env?") #,(nbpr "empty-env"))) any/c]{
The @nbr[expr] is evaluated with the specified @nbr[env].

@Interaction[
(simplisp
'(let ((add1 1) (sub1 2))
  (eval '(list add1 sub1) (current-env))))
(simplisp
'(let ((add1 1) (sub1 2))
  (eval '(list add1 sub1) empty-env)))]}

@(define force-comment (black @tt{(@nbpr{lazy} @nbr['yes])} " is not forced."))
@Elemtag{force}
@defproc[(force (promise any/c)) any/c]{
Fulfills a @nber["make-promise"]{promise}.
If @nbr[promise] is not a promise, it is returned as is.
If @nbr[promise] is a promise results depend on its @nbpr{promise-state}.

If the @nbpr{promise-state} is @nbr['delay] the state is changed to @nbr['running],
the thunk in its content is called,
the content is replaced by the value returned by the thunk
and this value is returned as the value of the @nbpr{force}-form
while the state is changed to @nbr['forced]. 
The result can be a promise.
If the thunk raises an exception, the content is replaced by this exception,
the state is set to @nbr['error] and finally the exception is raised again.

If the @nbpr{promise-state} is @nbr['lazy]
the @nbr[promise] is forced as described for state
@nbr['delay],@(lb)but if the result is a promise,
the latter is forced recursively in tail position.@(lb)
Hence, in fact a lazy promise is fulfilled more eagerly than a delayed promise.

If the @nbpr{promise-state} is @nbr['forced] the content is returned.

If the @nbpr{promise-state} is @nbr['error] the exception memorized in the content
is raised again.

If the @nbpr{promise-state} is @nbr['running] an exception is raised.

@Interaction[
(simplisp
'(let
  ((my-promise
    (make-promise #f 'delay
     (λ ()
      (display "my-promise is being forced. ")
      'some-value))))
  (display "step 1 ") (writeln (promise-state my-promise))
  (display "step 2 ") (writeln                my-promise)
  (display "step 3 ") (writeln (force         my-promise))
  (display "step 4 ") (writeln (promise-state my-promise))
  (display "step 5 ") (writeln (force         my-promise))))]

In a nest of delayed and lazy promises, forcing continues until:

@inset{
the result is not a promise or@(lb)
is a promise but not with state @nbr['lazy] or @nbr['delay] or@(lb)
the innermost promise that has been forced had state @nbr['delay].}

@Interaction[
(simplisp '(force (lazy (lazy (delay 'yes)))))
(code:line (simplisp '(force (delay (lazy 'yes)))) (code:comment #,force-comment))]

Forcing a running promise or a promise with @nbpr{promise-state} @nbr['error] yields an error:

@Interaction[
(define x (simplisp '(letrec ((x (lazy (force x)))) x)))
(simplisp `(promise-state ,x))
(code:line (simplisp `(force ,x)) (code:comment #,(red "Yields an error.")))
(simplisp `(promise-state ,x))
(code:line (simplisp `(force ,x)) (code:comment #,(red "Yields the same error again.")))]}

@Elemtag{global-define}
@defform*[#:kind "macro" ((global-define id expr)
                          (global-define (id arg ...) body-expr ...)
                          (global-define (id arg ...+ . arg) body-expr ...)
                          (global-define (id . arg) body-expr ...))]{
The first form enters the value of the @nbr[expr] under name @nbr[id] in the
@nbpr{current-global-table} and returns this value.
It is allowed to redefine an already existing binding in this table.
The @nbr[id] is locally bound within the @nbr[expr].
When applicable a name is @seclink["6"]{inferred}.
The second, third and fourth form are equivalent with:
@inset{
@tt{(@nbpr{global-define} @nbr[id] (@nbpr{lambda} (@nbr[arg] ...) @nbr[body-expr] ...))}@(lb)
@tt{(@nbpr{global-define} @nbr[id] (@nbpr{lambda} (@nbr[arg] ...+ . @nbr[arg]) @nbr[body-expr] ...))}
  @(lb)
@tt{(@nbpr{global-define} @nbr[id] (@nbpr{lambda} @nbr[arg] @nbr[body-expr] ...))}}}

@Elemtag{global-define-values}
@defmacro[(global-define-values (id ...) expr)]{
Multiple values version of @nbpr{global-define}.
The @nbr[id]s are locally bound within the @nbr[expr],
which must yield as many values as there are @nbr[id]s.
These values are returned as the values of the @nbpr{global-define-values}-form.

@Interaction[
(simplisp
'(global-define-values (even odd)
  (values
   (λ (n) (or (zero? n) (odd (sub1 n))))
   (λ (n) (and (not (zero? n)) (even (sub1 n)))))))
(simplisp '(map (global-ref even) '(0 1 2 3)))]}

@Elemtag{global-let}
@defmacro[(global-let (binding ...) expr ...)
           #:grammar ((binding id (local-id global-id)))]{
Binds local variables to the values of global variables.
A @nbr[binding] of the form @nbr[id] is interpreted as @nbr[(id id)].
In other respects the same as:
@inset{@tt{(@nbpr{let} ((@nbr[local-id] (@nbpr{global-ref} @nbr[global-id])) ...) @nbr[expr] ...)}}}

@Elemtag{global-ref}
@defmacro[(global-ref id)]{
Recollects the value of global variables @nbr[id] from the @nbpr{current-global-table}.
An exception is raised if the @nbr[id] cannot be found.}

@Elemtag{global-ref-values}
@defmacro[(global-ref-values id ...)]{
Recollects the values of global variables @nbr[id ...] from the @nbpr{current-global-table}
and returns the corresponding multiple value.}

@Elemtag{global-remove!}
@defmacro[(global-remove! id ...)]{
Removes the variables @nbr[id] @tt{...} from the @nbpr{current-global-table}.@(lb)
Returns @(Void). An @nbr[id] not present in the table does no harm.}

@Elemtag{global-set!}
@defmacro[(global-set! id expr)]{
Evaluates the @nbr[expr] and enters the value under name @nbr[id] in the @nbpr{current-global-table}.
@nb{No local} binding is created for the @nbr[id], but it may already have a local binding.
@nb{The @nbr[id]} must already have a global binding and its old value is returned.
Global and local bindings can coexist.

@Interaction[
(simplisp
'(let ((n 1))
  (global-define n undefined)
  (values
   (global-set! n (add1 n))
   (global-set! n 10)
   (global-ref n)
   n)))]

@Interaction[
(simplisp
'(let ((n 1))
  (global-define n undefined)
  (global-set! n (λ () n))
  (code:comment #,(black "within the closure " @green{@tt{n}} " retains the local binding."))
  ((global-ref n))))]}

@Elemtag{global-set!-values}
@defmacro[(global-set!-values (id ...) expr)]{
Multiple value version of @nbpr{global-set!}.
The @nbr[id]s must already been bound in the @nbpr{current-global-table}.
The new values are assigned and the old values are returned.}

@Elemtag{global-table?}
@defproc[#:kind "predicate" (global-table? (obj any/c)) boolean?]

@Elemtag{global-vars}
@defproc[(global-vars) (listof symbol?)]{
Returns a sorted list of all variables currently in the @nbpr{current-global-table}.

@Interaction[
(simplisp
'(global-define-values (a b c) (values 1 2 3))
'(parameterize*
  ((current-global-table (empty-global-table)))
  (displayln (global-vars)))
'(displayln (global-vars))
'(global-remove! a b and some unbound ids)
'(global-vars))]}

@Elemtag{if}
@defmacro[(if condition then-expr else-expr)]{
Like @nbr[if] of @(Rckt).}

@Elemtag{iflet}
@defmacro[(iflet id condition then-expr else-expr)]{
Same as
@tt{(@nbpr{let} @nbr[((id condition))] (@nbpr{if} @nbr[id] @nbr[then-expr] @nbr[ else-expr]))}@(lb)
The @nbr[id] is bound in @nbr[then-expr] and @nbr[else-expr].}

@Elemtag{lambda}
@defmacro[(lambda formals expr ...)
          #:grammar ((formals (id ...) (id ...+ . id) id))]{
Returns a procedure satisfying predicate @nbpr{closure?}.
The environment of the @nbpr{lambda}-form is captured in the procedure.
When the procedure is called, it receives evaluated actual arguments and
the captured environment is extended with bindings of the @nbr[id]s with the actual arguments,
possibly shadowing previously present bindings.
Subsequently the body @tt{@nbr[expr] ...} is evaluated using the extended environment.
The body can be empty in which case the procedure returns @(Void).
The @nbr[id]s are not necessarily distinct.
An @nbr[id] shadows all identical @nbr[id]s at its left.
Examples:

@Interaction[
(simplisp '((lambda (a a a . a) a) 0 1 2 3 4 5 6 7 8 9))
(simplisp '(write ((lambda ()))))]

@note{@nbrl[simplisp]{Simplisp}'s macro @nber["lambda"]{lambda}
does not support optional nor keyword arguments.}}

@Elemtag{lazy}
@defmacro[(lazy expr ...)]{
Same as
@tt[
"(" @nbpr{make-promise} " " @nbr[#f]" " @nbr['lazy]  " (" @nbpr{λ} " () " @nbr[expr] " ...))"].
                                                                                              
In the following two examples care is taken such as not to force a promise whose value
is not yet needed.

@Interaction[
(simplisp
'(letrec
  (code:comment #,(black "Print additions in order to check which ones are made."))
  ((plus (λ (n m) (printf "~s + ~s = ~s~n" n m (+ n m)) (+ n m)))
   (make-fibonacci
    (λ (first second)
     (cons first
      (lazy  (code:comment #,(black "Avoid the addition when not yet needed."))
       (make-fibonacci (force second)
        (lazy (plus first (force second))))))))
   (draw-fibonacci
    (λ (fibonacci n)
     (if (zero? n) '()
      (let ((fibonacci (force fibonacci)))
       (cons (car fibonacci)
        (draw-fibonacci (cdr fibonacci) (sub1 n))))))))
  (draw-fibonacci (make-fibonacci 0 1) 11)))]}

@Elemtag{let}
@defmacro[(let ((id expr) ...) body-expr ...)]{
The @nbr[expr]s are evaluated in the current environment.
Subsequently the @nbr[id]s are locally bound to the values of the @nbr[expr]s.
Finally the body @tt{@nbr[body-expr] ...} is processed with the new local bindings.
The body can be empty in which case the form returns
@(Void).
The @nbr[id]s are not necessarily distinct.
An @nbr[id] shadows all identical @nbr[id]s at its left.
Example:

@Interaction[
(simplisp '(let ((a 1) (a 2) (a 3)) a))]}

@Elemtag{let*}
@defmacro[(let* ((id expr) ...) body-expr ...)]{
Like @nbpr{let}, but nested. Each @nbr[expr] is evaluated after all @nbr[id]s at the left
already have been bound to the value of the corresponding @nbr[expr]. For example:

@Interaction[
(simplisp '(let* ((a 1) (a (add1 a)) (a (add1 a))) a))]}

@Elemtag{let*-values}
@defmacro[(let*-values (((id ...) expr) ...) body-expr ...)]{
Multiple value version of @nbpr{let*}.}

@Elemtag{let-values}
@defmacro[(let-values (((id ...) expr) ...) body-expr ...)]{
Multiple value version of @nbpr{let}.}

@Elemtag{let/cc}
@defmacro[(let/cc id expr ...)]{
Like @nbr[let/cc] of @(Rckt), but the body @tt{@nbr[expr] ...} can be empty,
in which case the value is
@(Void).}
        
@Elemtag{let/ec}
@defmacro[(let/ec id expr ...)]{
Like @nbr[let/ec] of @(Rckt), but the body @tt{@nbr[expr] ...} can be empty,
in which case the value is @(Void).}

@Elemtag{letrec}
@defmacro[(letrec ((id expr) ...) body-expr ...)]{
Like @nbpr{let}, but evaluating the @nbr[expr]s after all @nbr[id]s already have been bound
possibly shadowing already existing bindings.
After the @nbr[expr]s have been evaluated their values are assigned to the
corresponding @nbr[id]s in order from left to right.
Subsequently the @nbr[body-expr]s are evaluated. Allows recursion:

@Interaction[
(simplisp
'(letrec ((! (λ (n) (if (zero? n) 1 (* n (! (sub1 n)))))))
         (! 5)))
(code:comment #,(black "Mutual recursion:"))
(simplisp
'(letrec ((ev? (λ (n) (or (zero? n) (od? (sub1 n)))))
          (od? (λ (n) (and (not (zero? n)) (ev? (sub1 n))))))
         (map ev? (build-list 10 values))))]}

@Elemtag{letrec-values}
@defmacro[(letrec-values (((id ...) expr) ...) body-expr ...)]{
Multiple value version of @nbpr{letrec}.}

@Elemtag{local-vars}
@defproc[(local-vars) (listof symbol?)]{
Returns a list of the names of all locally bound variables. Example:

@Interaction[
(simplisp '(local-vars))
(simplisp '(let ((a 1) (b 2)) (local-vars)))]}

@(define macro-comment1
  (black "unquote " @nbpr{else} " for the caller may have bound it to " @nbr[#f] "."))
@(define macro-comment2
  (black "Unquote " @nbpr{let} " such as to insert the macro, not the symbol."))

@Elemtag{macro}
@defmacro[(macro formals expr ...)
          #:grammar ((formals (id ...) (id ...+ . id) id))]{
Produces a macro. It is like a procedure but it receives unevaluated arguments and
must return one value. This value is evaluated in the environment from which the macro is called.
(A simplified form of the macro-form of Common LISP.)
Preparation of a hygienic macro requires some caution.
Some examples:

@Interaction[
(simplisp
(code:comment #,(black "For hygiene: unquote " @nbpr{set!} "."))
'(let ((exchange (macro (x y) `(,set! ,x (,set! ,y ,x)))))
  (let ((a 1) (b 2))
   (println (list a b))
   (exchange a b)
   (println (list a b)))))]

@Interaction[
(simplisp
'(let
  ((let*-with-less-parentheses
    (macro (bindings . body)
    `(,let* (code:comment #,(black "For hygiene: not symbol " @nbr['let*] ", but the macro itself!"))
      ,(let loop ((bindings bindings))
        (if (null? bindings) '()
         (cons (list (car bindings) (cadr bindings))
          (loop (cddr bindings)))))
      ,@body))))
  (let*-with-less-parentheses
   (a 1
    b (add1 a)
    c (+ a b)
    d (* a b c))
   (list a b c d))))]

The above macro is not recursive, although it uses a loop for the construction of the expression
to be evaluated, in particular for the list of bindings.
In the following version the macro generates an expression that may call the macro recursively.

@Interaction[
(simplisp
'(letrec
  ((let*-with-less-parentheses
    (macro (bindings . body)
     (if (null? bindings) `(,begin ,@body)
     `(,let (code:comment #,macro-comment2)
       ((,(car bindings) ,(cadr bindings)))
       (,let*-with-less-parentheses ,(cddr bindings) ,@body))))))
  (let*-with-less-parentheses
   (a 1
    b (add1 a)
    c (+ a b)
    d (* a b c))
   (list a b c d))))]

More interesting is a macro that implements a realy new form.
@elemref["simplisp"]{Simplisp} does not provide a do-macro, but we can make it as follows:

@Interaction[
(simplisp
'(let
  ((do
    (macro (controls stop . body)
     (let
      ((vars (map car controls))
       (inits (map cadr controls))
       (steppers
        (map
         (λ (control)
          (if (null? (cddr control)) (car control)
           (caddr control)))
         controls))
       (stop (car stop))
       (finish (cdr stop))
       (loop (gensym))) (code:comment #,(black "Use " @nbr[gensym] " for hygiene."))
     `(,let ,loop ,(map list vars inits)
       (,cond
        (,stop ,@(if (null? finish) (list (void)) finish))
        (,else (code:comment #,macro-comment1)
         (,begin ,@body (,loop ,@steppers)))))))))
  (code:comment #,(black "A do-loop for fibonacci starting with 0 and 1."))
  (do ((i 0 (add1 i)) (a 0 b) (b 1 (+ a b)))
   ((> i 9)
    (printf "stopper-values: i=~s, a=~s b=~s~n" i a b)
   'stop)
   (printf "fibonacci ~s: ~s~n" i a))))]}

@Elemtag{macro?}
@defproc[#:kind "predicate" (macro? (x any/x)) boolean?]{
Returns @nbr[#t] if the argument is a predefined macro of @nbr[simplisp]
or a macro made with macro @nbpr{macro} or procedure @nbpr{make-macro}, else @nbr[#f].}

@Elemtag{make-macro}
@defproc[(make-macro (proc (-> list? #,(nbpr "env?") any/c))) #,(nbpr "macro?")]{
When the macro is called, the @nbr[proc] is called with two arguments,
the list of unevaluated arguments
and the @elemref["environment"]{environment} from which it is called.

@Interaction[
(simplisp 
'(global-define infix
  (make-macro 'infix
   (λ (uargs env)
    (eval `(,(cadar uargs) ,(caar uargs) ,(caddar uargs)) env)))))
(simplisp '((global-ref infix) (3 * 4)))
(code:comment #,(black "Let's check the hygiene:"))
(simplisp
'(let-values
  (((       eval list cadar caar caddar * three four)
    (values void void void  void void   + 3     4)))
  (code:comment #,(black "The rebinding of " @nbr[*] " to " @nbr[+] " must be noticed!"))
  ((global-ref infix) (three * four))))]}

@Elemtag{make-promise}
@defproc[
(make-promise (name (or/c #f symbol?))
              (state (or/c 'lazy 'delay 'forced))
              (content (case state
                        ((lazy delay) (-> any/c))
                        ((forced) any/c)))) promise?]{
A promise has @nber["promise-state"]{state} and @nbr[content].@(lb)
The @nbr[name] is used in the printed form of the promise.@(lb)
A promise is fulfilled by @nber["force"]{forcing} it.@(lb)
The state is @nbr['lazy], @nbr['delay], @nbr['running], @nbr['forced] or @nbr['error].@(lb)
If the state is @nbr['lazy] or @nbr['delay] the content is a thunk @nbr[(-> any/c)].@(lb)
If the state is @nbr['forced] the content is @nbr[any/c].@(lb)
If the state is @nbr['error] the content is an
@seclink["exn-model" #:doc '(lib "scribblings/reference/reference.scrbl")]{exception}.@(lb)
While a promise is being @nber["force"]{forced} its state is @nbr['running].}

@Elemtag{make-promise-type}
@defproc[(make-promise-type (name (or/c symbol? #f)))
         (values procedure? procedure? #,(nbpr "macro?") #,(nbpr "macro?") procedure?)]{
Creates a promise type.
The returned values are:

@inset{@Tabular[
(("Returned" "Is like predefined analogue")
 ("constructor"  @nbpr{make-promise})
 ("predicate"    @nbpr{promise?})
 ("macro"        @nbpr{delay})
 ("macro"        @nbpr{lazy})
 ("procedure"    @nbpr{force}))
#:sep (hspace 3)
#:row-properties '((top-border bottom-border) () () () () (bottom-border))]}

The @nbr[name] is used in printed forms of the promises, possibly together with inferred name.
The macros and procedures of distinct promise types do not recognize each other's promises.
Procedure @nbpr{promise-state} applies to all promise types.

@note{In fact the predefined analogues are
made by procedure @nber["make-promise-type"]{make-promise-type}.}}

@Elemtag{make-stream-type}
@defproc[(make-stream-type (name (or/c #f symbol?)))
         (values
          #,(nbpr "macro?")
          #,(nbpr "macro?")
          #,(nbpr "macro?")
          procedure?
          procedure?
          procedure?
          procedure?
          procedure?
          #,(tt (italic "value")))]{
Creates a stream type. The returned values are:

@inset{@Tabular[
(("Returned" "Is like predefined analogue")
 ("macro"     @nbpr{stream-lazy})
 ("macro"     @nbpr{stream-cons})
 ("macro"     @nbpr{stream})
 ("procedure" @nbpr{stream-car})
 ("procedure" @nbpr{stream-cdr})
 ("predicate" @nbpr{stream?})
 ("predicate" @nbpr{stream-null?})
 ("predicate" @nbpr{stream-pair?})
 (@tt{@italic{value}} @nbpr{stream-null}))
#:sep (hspace 3)
#:row-properties '((top-border bottom-border) () () () () () () () () (bottom-border))]}

The macros and procedures of distinct stream types do not recognize each other's streams.
The @nbr[name] is used in printed forms of the streams,
possibly together with an inferred name.

@note{In fact the predefined analogues are made by procedure
@nber["make-stream-type"]{make-stream-type}.}

Example: fibonacci stream:

@Interaction[
(simplisp
'(let-values
  (((_ strm-cons _ strm-car strm-cdr _ _ _ _)
    (make-stream-type 'strm)))
  (letrec
   ((strm-map
     (λ (proc . strms)
      (strm-cons
       (apply proc (map strm-car strms))
       (apply strm-map proc (map strm-cdr strms)))))
    (strm-take
     (λ (strm n)
      (if (zero? n) '()
       (cons (strm-car strm)
        (strm-take (strm-cdr strm) (sub1 n))))))
    (code:comment #,(black "Print additions to check that no more elements are forced than needed."))
    (plus (λ (n m) (printf "~s + ~s = ~s~n" n m (+ n m)) (+ n m)))
    (code:comment #,(black "A self referencing stream:"))
    (fibonacci
     (strm-cons 0
      (strm-cons 1
       (strm-map plus fibonacci (strm-cdr fibonacci))))))
   (strm-take fibonacci 11))))]}

@elemtag{null-form}
@deftogether[
(@defidform[#:kind "value" null]
 @defform[#:kind "special form" #:link-target? #f (null id)])]{
@nber["null-form"]{null} is a predefined variable containing the empty list.@(lb)
Because the empty list is self-evaluating, the special form can also be written as:
@inset{@nbr[(() id)]}
It retrieves the value of the @nbr[id] from the
@elemref["environment"]{clean environment} ignoring the local environment.
For example:

@Interaction[
(simplisp '(let ((add1 undefined) (null undefined)) (() add1)))]

In the following example all clean variables are shadowed by local ones.

@Interaction[
(simplisp
'(let*
  ((variables (clean-vars))
   (shadowed (string->uninterned-symbol "shadowed"))
   (shadowed? (λ (id) (eq? id shadowed)))
   (bindings (map (λ (var) `(,var ',shadowed)) variables)))
  (simplisp
  `(let ,bindings
    (code:comment #,(black "Check that all clean variables are shadowed."))
    (,unless (,andmap ,shadowed? (,list ,@variables))
     (,error "test fails"))
    (code:comment #,(black "In particular:"))
    (,printf "~a is ~s~n" "map  " map  )
    (,printf "~a is ~s~n" "add1 " add1 )
    (,printf "~a is ~s~n" "quote" quote)
    (code:comment #,(black "Nevertheless we can access them."))
    ((() map) (() add1) ((() quote) (1 2 3)))))))]

There are several tricks to bypass all local bindings. The simplest one is:
@inset{@tt{((() @nbpr{with-env}) @nbr[#f] @italic{expr} ...)}}}
            
@Elemtag{or}
@defproc*[#:kind "macro" (((or) #f) ((or (expr any/c) ... (last-expr any)) (or/c any/c any)))]{
The @tt{@italic{exprs}} are evaluated from left to right and the value of the first one that is not
@nbr[#f] is returned, skipping the evaluation of the remaining @tt{@italic{exprs}} and the
@nbr[last-expr].
If all @tt{@italic{exprs}} yield @nbr[#f], the @nbr[last-expr] is evaluated in tail position and
its value or multiple value is returned.}

@Elemtag{parameterize*}
@defmacro[(parameterize* ((param value) ...) expr ...)]{
Like in @(Rckt).}

@Elemtag{promise-state}
@defproc[(promise-state (promise any/c)) (or/c 'lazy 'delay 'forced 'error 'running #f)]{
Returns the state of a @nber["make-promise"]{promise} or @nbr[#f] if the argument is not a promise.

@Interaction[
(simplisp
'(letrec
  ((p
    (make-promise 'p 'lazy
     (λ ()
      (printf "state while being forced  : ~s~n" (promise-state p)) 'value))))
  (printf     "state before forcing      : ~s~n" (promise-state p))
  (printf     "value when forcing        : ~s~n" (force p))
  (printf     "state after forcing       : ~s~n" (promise-state p))
  (printf     "value after forcing again : ~s~n" (force p))))]}
                                                                     
@Elemtag{promise?}
@defproc[#:kind "predicate"
(promise? (x any/c)) boolean?]{
Predicate for @nber["make-promise"]{promises}.}

@(define quasiquote-comment
  (black "Read this as " @tt{((unquote 1) (unquote-splicing add1))}))
@Elemtag{quasiquote}
@defmacro[(quasiquote expr)]{
Simplified form of @(Rckt)'s @nbr[quasiquote].
Symbols @(tt "unquote") and @(tt "unquote-splicing") are recognized
independently of their bindings. They even don't need bindings.
In unquoted parts they are treated as the names of variables.
The quasiquotation is recursive for immutable pairs,
mutable and immutable vectors and mutable and immutable boxes.
Immutable vectors and boxes are converted to mutable ones.
For each pair, vector and box that is not part of
an object other than a pair, vector or box, a new one is constructed.
Therefore they are not @nbrl[eq?]{eq} to the original.

@tt{
@(hspace 3)(@nbr[simplisp]@(lb)
@(hspace 3)'(@nbpr{let} ((unquote-splicing 4) (unquote @nbr[add1]))@(lb)
@(hspace 4)`(a b unquote ,@tt["@"](@nbr[map] unquote '(0 1 2 3)) ,(unquote unquote-splicing))))}@(lb)
@(hspace 3)@blue{@tt{(a b unquote 1 2 3 4 5)}}

The following is exacly the same, but almost unreadable for the human eye because of
reader-abbreviation:

@Interaction[
(simplisp
'(let (,@4 ,add1)
 `(a b unquote ,@(map . ,'(0 1 2 3)) ,,unquote-splicing)))]}

@Elemtag{quote}
@defmacro[(quote datum)]{Returns the @nbr[datum] without evaluating it.}

@Elemtag{racket}
@defproc[(racket (form any) ...) any]{
Procedure @nbpr{racket} allows evaluation of forms by @(Rckt).
If no @nbr[form] is present, the value is @(Void).
Otherwise the @nbr[form]s are evaluated from left to right and the value or multiple value of the last
@nbr[form] is returned.
The @nbr[form]s are evaluated in a
@seclink["namespace-model" #:doc '(lib "scribblings/reference/reference.scrbl")]{namespace}
of @(Rckt) to which variable @nbr[simplisp] is added.
The namespace is part of the internal state of @nbr[simplisp]
and is preserved between successive calls to @nbr[simplisp].
Within a @nbr[form] one can switch back to @nbr[simplisp].
Switching forth and back can be nested to any desired depth.
@nbrl[simplisp]{Simplisp} and @nbpr{racket} live in separate worlds.
They do not share variables, although they do share many objects. Example:

@Interaction[
(code:comment #,(black "Separation of variables:"))
(simplisp
'(let ((add1 10))
  (racket '(define add1 20))
  (list add1 (racket 'add1))))]

@Interaction[
(code:comment #,(black "Sharing objects:"))
(eq? (simplisp 'add1) add1)
(eq? (simplisp '(racket 'add1)) add1)]

Procedure @nbpr{racket} accepts @nbr[require]-forms, for example:

@Interaction[
(code:line (simplisp '(racket 'take)) (code:comment #,(red "Fails!")))]

It appears that procedure @nbr[take] is not in a
@seclink["namespace-model" #:doc '(lib "scribblings/reference/reference.scrbl")]{base namespace}.
We can import it:

@Interaction[
(simplisp '(racket '(require (only-in racket take))))
(simplisp '((racket 'take) '(0 1 2 3 4 5) 3))]}

@Elemtag{racket-reset}
@defproc[(racket-reset) void?]{
Resets the
@seclink["namespace-model" #:doc '(lib "scribblings/reference/reference.scrbl")]{namespace}
used by @nbpr{racket}.

@Interaction[
(simplisp '(racket '(define add1 10)))
(simplisp '(racket 'add1))
(simplisp '(racket-reset))
(simplisp '(racket 'add1))]}

@Elemtag{reset}
@defproc[(reset) void?]{
Resets the @seclink["4"]{internal state} of @nbr[simplisp].}

@Elemtag{set!}
@defmacro[(set! id expr)]{
Assigns the value of the @nbr[expr] to variable @nbr[id],
which must already be locally bound.
The old value of the variable is returned.
If @nbr[id] has no local binding, an exception is raised.

@Interaction[
(simplisp
'(let ((a 1) (b 2)) (set! a (set! b a)) (list a b)))
(code:comment #,(black "Attempt to assign to a clean variable fails."))
(simplisp '(set! list 'redefined))
(code:comment #,(black "Attempt to assign to an unbound variable fails."))
(simplisp '(set! a 1))]
The @nbr[expr] is evaluated in the local environment of the @nbpr{set!}-form.

@Interaction[
(simplisp
'(let ((! undefined))
  (set! !
   (λ (n)
    (if (zero? n) 1 (* n (! (sub1 n))))))
  (! 5)))]}

@Elemtag{set!-values}
@defmacro[(set-values! (id ...) expr)]{Multiple values version of @nbpr{set!}.
The @nbr[id]s must be locally bound.
The @nbr[expr] must produce as many values as there are @nbr[id]s in the form.
The values are assigned and the old values are returned.
If an @nbr[id] has multiple occurrences in the list of @nbr[id]s,
the value corresponding to the rightmost occurrence is stored.}

@defproc[#:link-target? #f
(simplisp (top-expr #,(black @nbr[any/c] " evaluating to " @nbr[any])) ... ) any]{
See @nbr[simplisp].}

@Elemtag{special-vars}
@defproc[(special-vars) (listof symbol?)]{
Returns a sorted list of all symbols naming the clean variables of @nbr[simplisp]
that are not @elemref["clean-vars"]{borrowed} from @(Rckt).
See @elemref["simplisp-variables"]{the list above.}}

@(define (SRFI41) @hyperlink["https://srfi.schemers.org/srfi-41/srfi-41.html"]{SRFI 41})

@Elemtag{stream}
@defmacro[(stream expr ...)]{See @(SRFI41).}

@Elemtag{stream-car}
@defproc[(stream-car (strm #,(nbpr "stream-pair?"))) any/c]{See @(SRFI41).}

@Elemtag{stream-cdr}
@defproc[(stream-cdr (strm #,(nbpr "stream-pair?"))) any/c]{See @(SRFI41).}

@Elemtag{stream-cons}
@defmacro[(stream-cons kar kdr)
 #:contracts
  ((kar any/c)
   (kdr #,(nbpr "stream?")))]{See @(SRFI41).}

@Elemtag{stream-lazy}
@defmacro[(stream-lazy expr ...)]{
Same as @tt{
(@(nbpr "stream-cdr")
 (@(nbpr "stream-cons")
  ignore
  (@(nbpr "begin")
    @(nbr expr) ...)))},
but without forming an intermediate stream-pair.}

@Elemtag{stream-null}
@defthing[#:kind "value" null-stream #,(nbpr "stream-null?")]{See @(SRFI41).}

@Elemtag{stream-null?}
@defproc[#:kind "predicate" (stream-null? (obj any/c)) boolean?]{See @(SRFI41).}

@Elemtag{stream-pair?}
@defproc[#:kind "predicate" (stream-pair? (obj any/c)) boolean?]{See @(SRFI41).}

@Elemtag{stream?}
@defproc[#:kind "predicate" (stream? (obj any/c)) boolean?]{See @(SRFI41).}

@Elemtag{trace}
@defmacro[(trace expr ...)]{
Same as @tt{(@nbpr{parameterize*} ((@nbpr{trace-option} @nbr[#t])) @nbr[expr] ...)}}

@Elemtag{trace-align}
@defparam[trace-align width natural? #:value 3]{
In a @nber["trace-option"]{trace report} a line may start with a natural number identifying
the subexpression it refers to.
The identification is right justified in a field of at least @nbr[width] characters.
@nb{If more} characters are needed than the specified width, the number is not truncated.}

@Elemtag{trace-assgn}
@defparam*[trace-assgn on/off any/c boolean? #:value #f]{See @nbpr{trace-option}.}

@Elemtag{trace-finis}
@defparam*[trace-finis on/off any/c boolean? #:value #f]{See @nbpr{trace-option}.}

@Elemtag{trace-option}
@defparam*[trace-option details
 (or/c
  boolean?
  'all
  (listof (or/c 'start 'finis #;'value #;'varef 'selfi #;'assgn)))
 boolean? #:value #f]{
Controls tracing.
When @nber["trace-option"]{tracing} is enabled,
every tail is followed by some tracing action and consequently the tail looses its tail position.
The details of tracing are controlled by the parameters:

@inset{
  @nbpr{trace-start}@(lb)
  @nbpr{trace-finis}@(lb)
@;  @nbpr{trace-value}@(lb)
@;  @nbpr{trace-varef}@(lb)
@;  @nbpr{trace-assgn}@(lb)
  @nbpr{trace-selfi}}

When called with the @nbr[details]-argument, the @nbpr{trace-option} does the following:

@inset{@Tabular[
((@nbr[details]       "Action")
 (@nbr[#f]            "Tracing off without affecting the detail parameters.")
 (@nbr[#t]            "Tracing on if at least one detail parameter is enabled,")
 (""                  "else tracing off. Does not affect the detail parameters.")
 (@nbr['all]          "Tracing on and enables all detail parameters.")
 (@nbr[()]            "Tracing off and disables all detail parameters.")
 (@nb{Non-empty list} "Tracing on, enables all selected, disables all omitted details."))
#:sep (hspace 3)
#:row-properties '((top-border bottom-border) () () () () () (top bottom-border))]}
  
When tracing is enabled, a report of the evaluation is written on the @nbr[current-output-port].
Every subexpression is assigned a distinct number for identification.
A subexpression that is evaluated more than once,
for example the body of a recursive procedure,
each time gets a new number.

@itemlist[
@item{If @nbpr{trace-option} and @nbpr{trace-start} are enabled,
every subexpression is shown in a start-line when its evaluation is initiated.}

@item{If @nbpr{trace-option} and @nbpr{trace-finis} are enabled,
a subexpression is shown again after completion of its evaluation,
except when @nbpr{trace-start} is enabled and the finis-line would follow the
corresponding start-line without an intervening start-line.}

@item{If @nbpr{trace-option} and @nbpr{trace-value} are enabled,
the value or multiple value of the evaluated subexpression is shown.}

@item{If @nbpr{trace-option} and @nbpr{trace-varef} are enabled,
every variable reference is shown with the name of the variable and its value.}

@item{If @nbpr{trace-option} and @nbpr{trace-assgn} are enabled,
every assignment is reported with the name of the variable and the assigned value.}

@item{If @nbpr{trace-option} and @nbpr{trace-selfi} are enabled,
every self-evaluating object is reported.}]

Start-lines, finis-lines and value-lines show the identification of the subexpression they refer to
and the subexpression cq value.
A start-line is marked with @tt{START}, a finis-line with @tt{FINIS} and
a value-line with @tt{VALUE} for a single value, @tt{MULTV} in case of a multiple value or
@tt{NOVAL} in case there are no values. Notice that @(Void) is a value.
@nb{A variable} reference is marked with @tt{VAREF},
an assignment with @tt{ASSGN} and a self-evaluating object with @tt{SELFI}.

Example: 

@Interaction[
(simplisp
'(parameterize* ((trace-width 65) (trace-option 'all))
  (let* ((a 11) (b (* 2 a)))
   (set! a (+ a b))
   (values a b (+ a b)))))]}

@Elemtag{trace-selfi}
@defparam*[trace-selfi on/off any/c boolean? #:value #f]{See @nbpr{trace-option}.}

@Elemtag{trace-start}
@defparam*[trace-start on/off any/c boolean? #:value #f]{See @nbpr{trace-option}.}

@Elemtag{trace-value}
@defparam*[trace-value on/off any/c boolean? #:value #f]{See @nbpr{trace-option}.}

@Elemtag{trace-varef}
@defparam*[trace-varef on/off any/c boolean? #:value #f]{See @nbpr{trace-option}.}

@Elemtag{trace-width}
@defparam[trace-width width (or/c #f natural?) #:value #f]{
In a @nber["trace-option"]{trace report} each line is limited to @nbr[width] characters.
A line longer than @nbr[width] characters is truncated, replacing the last four characters to
‘@tt{ ...}’. A @nbr[width] less than 4 has the same effect as @nbr[width] 4.
@nb{@nbr[width] @nbr[#f]} indicates that the lines must not be cut off.}

@Elemtag{undefined}
@defthing[undefined #,(nbpr "undefined?")]{
Variable containing a unique value.
In particular used to bind variables before their values are known. Example:

@Interaction[
(simplisp '(letrec ((a a)) a))]}

@Elemtag{undefined?}
@defproc[#:kind "predicate" (undefined? (arg any/c)) boolean?]{
@nbr[#t] if the argument is the unique object @nbpr{undefined}, else @nbr[#f].}

@Elemtag{unless}
@defmacro[(unless condition expr ...)]{
Same as in @(Rckt), but the body @tt{@nbr[expr] ...} may be empty, in which case the form returns
@(Void).}

@Elemtag{when}
@defmacro[(when condition expr ...)]{
Same as in @(Rckt), but the body @tt{@nbr[expr] ...} may be empty, in which case the form returns
@(Void).}

@Elemtag{with-env}
@defmacro[(with-env env expr ...)
#:contracts ((env (or/c #t #f 'copy #,(nbpr "env?"))))]{
Same as @nb{(@nbpr{begin} @nbr[expr] @(tt "..."))} but each @nbr[expr] evaluated in the following
@elemref["environment"]{environment}:

@inset{@Tabular[((@nbr[env]   "environment")
                 (@nbr[#f]    @elemref["empty-env"]{empty local environment})
                 (@nbr[#t]    @elemref["current-env"]{the current environment})
                 (@nbr['copy] @elemref["copy-env"]{a copy of the current environment})
                 (@nbpr{env?} "this environment.")) #:sep (hspace 2)
                #:row-properties '(bottom-border ()()()())]}}

@Elemtag{with-handlers*}
@defmacro[(with-handlers* ((predicate handler) ...) expr ...)]{
Same as in @(Rckt), but the body @tt{@nbr[expr] ...} may be empty,
in which case the form returns @(Void).}

@Elemtag{λ}
@defthing[#:kind "macro" λ #,(nbpr "macro?")]{
Synonym of @nbpr{lambda}.}
       
@(bold (larger (larger "The end")))

@(letrec
  ((insert
    (λ (e lst)
     (cond
      ((null? lst) (list e))
      ((symbol<? (car lst) e) (cons (car lst) (insert e (cdr lst))))
      (else (cons e lst))))))
  (let* ((documented
          (insert 'simplisp (reverse (remove 'simplisp (map string->symbol (add-elem-tag))))))
         (documented-and-sorted (sort documented symbol<?))
         (defined Special-vars))
   (printf "~nSpecial vars sorted: ~s ~n" (if (equal? documented-and-sorted documented) 'ok 'WRONG))
   (printf "~nList of documented variables~n~n")
   (for-each displayln documented-and-sorted)
   (let ((x (remove* defined documented-and-sorted)))
    (if (null? x)
     (printf "~nAll documented variables are implemented.~n")
     (printf "~nVariables documented but not implemented:~n~n~s~n~n" x)))
   (let ((x (remove* documented-and-sorted defined)))
    (if (null? x)
     (printf "~nAll implemented variables are documented.~n~n")
     (printf "~nVariables implemented but not documented:~n~n~s~n~n" x)))))
