#lang racket/base

(provide simplisp source-code)

(define-syntax-rule
 (define-with-source-code (simplisp source-code)         expr        )
 (define-values           (simplisp source-code) (values expr 'expr)))

(define-with-source-code (simplisp source-code)

 (letrec-values

  (((register) '())

   ((register-put!)
    (λ (name renamer object)
     (set! register (cons (list name renamer object) register))
     object))

   ((fill-clean-environment!) ; Places special variables in the clean-environment and
    (λ ()                     ; returns a sorted list of the names of all special variables.
     (for-each enter-in-clean-environment! register)
     ; At the end of the source-code the following will become garbage:
     ;    register
     ;    register-put!
     ;    fill-clean-environment!
     ;    enter-in-clean-environment!
     ;    rename
     ;    clean-set!
     ; clean-set! is the only procedure mutating the clean-namespace.
     ; Return sorted list of all special variables (for procedure $special-vars)
     (sort (map car register) symbol<?)))

   ((enter-in-clean-environment!)
    (λ (entry)
     (let ((name (car entry)) (renamer (cadr entry)) (object (caddr entry)))
      (clean-set! name (rename name renamer object)))))

   ((rename) ; Also converts macro-procs to macros.
    (λ (name renamer object)
     (case renamer
      ((#f) object)
      ((proc) (procedure-rename object name))
      ((super) (set-super-type-name! object name) object)
      ((macro) ($make-macro name object))
      (else (error 'source-code "unknown renamer: ~s~n  for variable: ~s" renamer name)))))

   ((clean-environment) (make-base-namespace))

   ((clean-ref)
    (λ (id)
     (namespace-variable-value id #t
      (λ () (error 'varef "unbound variable: ~s" id))
      clean-environment)))

   ((clean-set!)
    (λ (id value)
     (namespace-set-variable-value! id value #t clean-environment #t)))

   (($clean-vars)
    (register-put! 'clean-vars 'proc ; A thunk, because we must wait until
     (λ ()                           ; fill-clean-environment! has stored the special variables.
      (let ((not-found (string->uninterned-symbol "not-found")))
       (sort
        (filter
         (λ (var)
          (not
           (eq?
            (namespace-variable-value var #t (λ () not-found) clean-environment)
            not-found)))
         (namespace-mapped-symbols clean-environment))
         symbol<?)))))

   (($special-vars)
    (register-put! 'special-vars 'proc ; A thunk, because we must wait until fill-clean-environment!
     (λ () special-var-names)))        ; has assigned the list special-var-names.

   (($borrowed-vars)
    (register-put! 'borrowed-vars 'proc
     (λ () (remove* ($special-vars) ($clean-vars)))))

   (($local-vars)
    (register-put! 'local-vars 'macro
     (λ (uargs env)
      (sort (hash-keys (env-hash env)) symbol<?))))

   ((racket-eval $make-list $call-with-values)
    (let* ((racket-eval (clean-ref 'eval)))
     (parameterize* ((current-namespace clean-environment)) ; Use racket-eval here for the require-
      (racket-eval '(require (only-in racket make-list))))  ; form, for in simplisp eval is another
     (clean-set! 'sort sort)                                ; procedure. Using racket's eval here
     (clean-set! 'apply apply)                              ; would destroy meta-recursivity.
     (clean-set! 'call-with-values call-with-values)
     (register-put! 'else #f 'else)
     (values racket-eval (clean-ref 'make-list) (clean-ref 'call-with-values))))
   
   ((trace-option-guard)
    (λ (x)
     (case x
      ((#f) x)
      ((#t)
       (or
        ($trace-start)
        ($trace-finis)
        ($trace-value)
        ($trace-assgn)
        ($trace-varef)
        ($trace-selfi)))
      ((all ())
       (let ((on/off (eq? x 'all)))
        ($trace-start on/off)
        ($trace-finis on/off)
        ($trace-value on/off)
        ($trace-varef on/off)
        ($trace-assgn on/off)
        ($trace-selfi on/off)
        on/off))
      (else
       (unless (and (list? x) (null? (remove* '(start finis value varef selfi assgn) x)))
        (error 'trace-option "incorrect argument: ~s" x))
       ($trace-start (member 'start x))
       ($trace-finis (member 'finis x))
       ($trace-value (member 'value x))
       ($trace-varef (member 'varef x))
       ($trace-assgn (member 'assgn x))
       ($trace-selfi (member 'selfi x))
       #t))))

   (($trace-option)
    (register-put! 'trace-option #f
     (make-parameter #f trace-option-guard 'trace-option)))

   ((inspector) (make-sibling-inspector))

   ((super-type super-type? super-type-name set-super-type-name!)
    (let*-values
     (((descr constr pred acc mut)
       (make-struct-type
        'super-type ; type-name
        #f          ; no super-type
        1           ; fields: name (other fields to be provided by sub-types)
        0           ; no auto fields
        #f          ; auto value n.a.
        (list
         (cons prop:object-name
          (λ (obj) (parameterize* (($trace-option #f)) (super-type-name obj)))))
        inspector
        #f          ; no procedure properety
        '()         ; mutable name
        #f)))       ; no guard
     (values descr pred
      (make-struct-field-accessor acc 0 'name)
      (make-struct-field-mutator  mut 0 'name))))

   ((make-closure $closure?)
    (let*-values
     (((printer)
       (λ (obj port mode)
        (parameterize* (($trace-option #f))
         (fprintf port "#<closure:~s>" (super-type-name obj)))))
      ((descr constr pred acc mut)
       (make-struct-type
        'closure ; type-name
        super-type
        1        ; nr of fields: procedure, name already in super-type
        0        ; no auto fields
        #f       ; auto-value n.a.
        (list (cons prop:custom-write printer))
        inspector
        0        ; procedure property
        '(0)     ; immutable proc
        #f)))    ; no guard
     (values constr (register-put! 'closure? #f pred))))

   (($make-macro $macro? macro-proc)
    (let*-values
     (((printer)
       (λ (macro port mode)
        (parameterize* (($trace-option #f))
         (fprintf port "#<macro:~s>" (super-type-name macro)))))
      ((guard)
       (λ (name macro-proc ignore)
        (unless (or (symbol? name) (not name))
         (raise-argument-error 'make-macro
          "(or/c symbol? #f)" 0 name macro-proc))
        (unless (and (procedure? macro-proc) (procedure-arity-includes? macro-proc 2))
         (raise-argument-error 'make-macro
          "(procedure-arity-include/c 2)" 1 name macro-proc))
        (values name macro-proc)))
      ((descr constr pred acc mut)
       (make-struct-type
        'macro ; type-name
        super-type
        1      ; nr of fields: macro-proc, name already in super-type
        0      ; no auto fields
        #f     ; auto-value n.a.
        (list (cons prop:custom-write printer))
        inspector
        #f     ; not a procedure
        '(0)   ; immutable macro-proc field
        guard)))
     (values
      (register-put! 'make-macro #f constr)
      (register-put! 'macro? #f pred)
      (make-struct-field-accessor acc 0 'proc))))

   ((env-type make-env $env? env-hash)
    (let*-values
     (((printer)
       (λ (env port mode)
        (parameterize* (($trace-option #f))
         (fprintf port "#<env:~s>" (super-type-name env)))))
      ((env-proc)
       (λ (env id . wrapped-optional-value)
        (cond
         ((null? wrapped-optional-value) (env-ref env id))
         (else (env-set! env id (car wrapped-optional-value))))))
      ((descr constr pred acc mut)
       (make-struct-type
        'env  ; type-name
        super-type ;
        1     ; nr of fields, immutable hasheq, name already in super-type
        0     ; no auto fields
        #f    ; auto-value n.a.
        (list (cons prop:custom-write printer))
        inspector
        env-proc
        '()   ; immutable hasheq
        #f))) ; no guard
     (values descr constr
      (register-put! 'env? #f pred)
      (make-struct-field-accessor acc 0 'hash))))

   (($empty-env $empty-env?)
    (let*-values
     (((printer)
       (λ (env port mode)
        (parameterize* (($trace-option #f))
         (fprintf port "#<empty-env>"))))
      ((descr constr pred acc mut)
       (make-struct-type
        'empty-env
        env-type ; super-type
        0        ; nr of fields, name and hasheq already in env-type
        0        ; no auto fields
        #f       ; auto value n.a.
        (list (cons prop:custom-write printer))
        inspector
        #f       ; procedure property already in env-type
        '()      ; no fields to be declared immutable
        #f)))    ; no guard
     (values
      (register-put! 'empty-env  #f (constr 'empty-env (hasheq)))
      (register-put! 'empty-env? #f pred))))

   ((env-ref)
    (λ (env id)
     (let ((bx (hash-ref (env-hash env) id #f)))
      (cond
       (bx (unbox bx))
       (else (clean-ref id))))))

   ((env-set!)
    (λ (env id value)
     (let ((bx (hash-ref (env-hash env) id #f)))
      (cond
       (bx
        (let ((old (unbox bx)))
         (when ($trace-assgn)
          (print-truncated "~a : ASSGN : ~s = ~s <- ~a = ~a" #f id value id old))
          (set-box! bx (infer-name id value)) old))
       (else (error 'assignment
        "allowed for locally bound vars only~n  var : ~s~n  value : ~s"
        id value))))))

   ((extend-env) ; Used by closures, macros made by macro macro and all binding forms.
    (λ (env formals actuals)
     (make-env #f
      (apply hash-set* (env-hash env)
       (let loop ((vars formals) (vals actuals))
        (cond
         ((symbol? vars) (list vars (box vals)))
         ((and (null? vars) (null? vals)) '())
         ((null? vars)
          (error 'application
           "too many actual args~n  formals: ~s~n  actuals: ~s"
           formals actuals))
         ((null? vals)
          (error 'application
           "unsatisfied formal args~n  formals: ~s~n  actuals: ~s"
           formals actuals))
         (else
          (cons (car vars)
           (cons (box (infer-name (car vars) (car vals)))
            (loop (cdr vars) (cdr vals)))))))))))

   ((infer-name)
    (λ (name obj)
     (when (and (super-type? obj) (not (super-type-name obj))) (set-super-type-name! obj name))
     obj))

   (($copy-env)
    (register-put! 'copy-env 'proc
     (λ (env)
      (make-env #f
       (make-immutable-hasheq
        (hash-map (env-hash env)
         (λ (var bx) (cons var (box (unbox bx))))))))))

   (($undefined $undefined?)
    (let*-values
     (((printer)
       (λ (ignore port mode)
        (parameterize* (($trace-option #f))
         (fprintf port "#<undefined>"))))
      ((descr constr pred acc mut)
       (make-struct-type
        'undefined ; name
        #f         ; no super-type
        0          ; no fields
        0          ; no auto fields
        #f         ; auto-value n.a.
        (list (cons prop:custom-write printer))
        inspector
        #f         ; not a procedure
        '()        ; immutability n.a.
        #f)))      ; no guard
     (values
      (register-put! 'undefined #f (constr))
      (register-put! 'undefined? #f pred))))

   ((promise-type $promise-state promise-content set-promise-state! set-promise-content!)
    (letrec-values
     (((guard)
       (λ (name state content ignore)
        (unless (or (symbol? name) (not name))
         (raise-argument-error 'make-promise "(or/c symbol? #f)" name))
        (case state
         ((lazy delay)
          (unless (and (procedure? content) (procedure-arity-includes? content 0))
           (error 'make-promise
            "with state ~s a thunk is expected, but found: ~s"
            state content)))
         ((forced) 'ok)
         (else (error 'make-promise "state must be 'lazy, 'delay or 'forced, given: ~s" state)))
        (values name state content)))
      ((descr constr pred acc mut)
       (make-struct-type
        'promise    ; name
        super-type  ; no super-type type
        2           ; fields: state and content, name already in super-type.
        0           ; no auto fields
        #f          ; auto value n.a.
        '()         ; no properties
        inspector
        #f          ; not a procedure
        '()         ; all fields mutable
        guard)))
     (values descr
      (register-put! 'promise-state #f (make-struct-field-accessor acc 0 'state))
      (make-struct-field-accessor acc 1 'content)
      (make-struct-field-mutator mut 0 'state)
      (make-struct-field-mutator mut 1 'content))))

   (($make-promise-type)
    (register-put! 'make-promise-type 'proc
     (λ (name)
      (letrec-values
       (((descr constr pred acc mut)
         (let*-values
          (((printer)
            (λ (promise port mode)
             (parameterize* (($trace-option #f))
              (fprintf port "#<~s:~s:~s>" name
               (super-type-name promise) ($promise-state promise))))))
          (make-struct-type
           name
           promise-type   ; super-type
           0              ; name, state and content in super-type
           0              ; no auto-fields
           #f             ; auto-value irrelevant
           (list (cons prop:custom-write printer))
           inspector
           #f             ; no procedure property
           '()            ; mutable
           #f)))          ; guard already in super-typetype *promise-decr
        ((lazy)
         ($make-macro (make-name name ":" 'lazy)
          (λ (exprs env)
           (constr #f 'lazy (λ () (@begin exprs env))))))
        ((delay)
         ($make-macro (make-name name ":" 'delay)
          (λ (exprs env)
           (constr #f 'delay (λ () (@begin exprs env))))))
        ((force) (λ (p) (selective-force p pred))))
       (values
        constr
        pred
        delay
        lazy
        (procedure-rename force (make-name "force-" name)))))))

   ((make-name)
    (λ elements
     (string->symbol (apply string-append (map ->string elements)))))

   ((->string) (λ (x) (if (string? x) x (symbol->string x))))

   ((selective-force)
    (λ (p pred)
     (cond
      ((not (pred p)) p) ; Force promises of own type only.
      (else
       (case ($promise-state p)
        ((running) (error 'force "promise to be forced is already being forced: ~s" p))
        ((error) (raise (promise-content p)))
        ((forced) (promise-content p))
        ((delay) (let ((v (run-promise p))) (set-promise! p v)))
        ((lazy)
         (let loop ((v (run-promise p)))
          (if (pred v)
           (if (eq? ($promise-state v) 'lazy)
            (begin
             (set-promise-content! p (promise-content v))
             (set-promise-state!   p ($promise-state  v))
             (set-promise-content! v (promise-content p))
             (set-promise-state!   v ($promise-state  p))
             (loop (run-promise p)))
            (set-promise! p (selective-force v pred)))
           (set-promise! p v))))
        (else (promise-content p)))))))

    ((set-promise!)
     (λ (p v)
      (set-promise-content! p v)
      (set-promise-state! p 'forced)
      v))

    ((promise-exn-handler)
     (λ (p)
       (λ (exn)
        (set-promise-state! p 'error)
        (set-promise-content! p exn)
        exn)))

    ((run-promise)
     (λ (p)
      (set-promise-state! p 'running)
      (call-with-exception-handler (promise-exn-handler p) (promise-content p))))

   ((ignore-0)
    (let-values
     (((make-promise promise? delay lazy force) ($make-promise-type 'promise)))
     (register-put! 'make-promise #f make-promise)
     (register-put! 'promise? #f promise?)
     (register-put! 'delay 'super delay)
     (register-put! 'lazy  'super lazy)
     (register-put! 'force 'proc force)))

   (($make-stream-type)
    (register-put! 'make-stream-type 'proc
     (λ (type-name)
      (letrec-values
       (((make-stream stream? stream-delay stream-lazy stream-force)
         ($make-promise-type type-name))
        ((make-elem elem? elem-delay elem-lazy elem-force)
         ($make-promise-type 'stream-element))
        ((stream-null) (make-stream 'null 'forced '()))
        ((stream-null?)
         (procedure-rename
          (λ (strm) (and (stream? strm) (null? (stream-force strm))))
          (make-name type-name '-null?)))
        ((stream-pair?)
         (procedure-rename
          (λ (strm) (and (stream? strm) (pair? (stream-force strm))))
          (make-name type-name '-pair?)))
        ((stream-cons)
         ($make-macro (make-name type-name '-cons)
          (λ (uargs env)
           (make-stream #f 'forced
            (cons
             (make-elem #f 'lazy (λ () (*eval (car uargs) env)))
             (make-stream #f 'lazy (λ () (*eval (cadr uargs) env))))))))
        ((stream)
         ($make-macro (if (eq? type-name 'stream) 'stream (make-name type-name '-stream))
          (λ (uargs env)
           (cond
            ((null? uargs) stream-null)
            (else
             (stream-cons
              (make-elem #f 'lazy (λ () (*eval (car uargs) env)))
              (stream (cdr uargs) env)))))))
        ((stream-car-name) (make-name type-name '-car))
        ((stream-car)
         (procedure-rename
          (λ (strm)
           (unless (stream? strm)
            (error stream-car-name "~s expected, given: ~s" type-name strm))
           (let ((content (stream-force strm)))
            (unless (pair? content)
             (error stream-car-name "non empty ~s expected, given ~s" type-name strm))
            (elem-force (car content))))
          stream-car-name))
        ((stream-cdr-name) (make-name type-name '-cdr))
        ((stream-cdr)
         (procedure-rename
          (λ (strm)
           (unless (stream? strm)
            (error stream-cdr-name "~s expected, given: ~s" type-name strm))
           (let ((content (stream-force strm)))
            (unless (pair? content)
             (error stream-cdr-name "non empry ~s expected, given ~s" type-name strm))
            (cdr content)))
          stream-cdr-name)))
       (values
        stream-lazy
        stream-cons
        stream
        stream-car
        stream-cdr
        stream?
        stream-null?
        stream-pair?
        stream-null)))))

   ((ignore-1)
    (let-values
     (((stream-lazy
        stream-cons
        stream
        stream-car
        stream-cdr
        stream?
        stream-null?
        stream-pair?
        stream-null)
       ($make-stream-type 'stream)))
     (register-put! 'stream-lazy  #f stream-lazy)
     (register-put! 'stream-cons  #f stream-cons)
     (register-put! 'stream       #f stream)
     (register-put! 'stream-car   #f stream-car)
     (register-put! 'stream-cdr   #f stream-cdr)
     (register-put! 'stream?      #f stream?)
     (register-put! 'stream-null? #f stream-null?)
     (register-put! 'stream-pair? #f stream-pair?)
     (register-put! 'stream-null  #f stream-null)))

   ((simplisp)
    (register-put! 'simplisp #f
     (procedure-rename
      (λ exprs
       (cond
        ((null? exprs) (void))
        (else
         (let loop ((expr (car exprs)) (exprs (cdr exprs)))
          (set! trace-id 0)
          (cond
           ((null? exprs) (*eval expr $empty-env))
           (else (*eval expr $empty-env) (loop (car exprs) (cdr exprs))))))))
     'simplisp)))

   ((*eval)
    (λ (expr env)
     ((if ($trace-option) tracing-eval normal-eval) expr env)))

   ((evallist)
    (λ (uargs env)
     (let loop ((uargs uargs) (vals '()))
      (cond
       ((null? uargs) (reverse vals))
       (else (loop (cdr uargs) (cons (*eval (car uargs) env) vals)))))))

   ((evalargs)
    (λ (uargs env)
     (let loop ((uargs uargs) (vallists '()))
      (cond
       ((null? uargs) (apply append (reverse vallists)))
       (else
        (loop (cdr uargs)
         (cons ($call-with-values (λ () (*eval (car uargs) env)) list) vallists)))))))

   ((normal-eval)
    (λ (expr env)
     (cond
      ((symbol? expr) (env expr))
      ((and (pair? expr) (list? expr)) (apply-op (*eval (car expr) env) (cdr expr) env))
      (else expr))))

   ((apply-op)
    (λ (op uargs env)
     (cond
      ((procedure? op) (apply op (evalargs uargs env)))
      (($macro? op) ((macro-proc op) uargs env))
      ((null? op) (apply-null uargs))
      (else
       (error 'application
        (string-append
         (format "operator expected, given ~s~n" op)
         (if (null? uargs) "  no arguments"
          (apply string-append
           (format "  ~s arguments" (length uargs))
           (map (λ (i uarg) (format "~n  unevaluated arg ~s: ~s" i uarg))
            (build-list (length uargs) add1) uargs)))))))))

   ((apply-null) ; Retrieves a value from the clean environment.
    (λ (uargs)
     (check-null-form uargs)
     (clean-ref (car uargs))))

   ((tracing-eval)
    (λ (expr env)
     (cond
      ((symbol? expr)
       (let ((value (env expr)))
        (when (and ($trace-option) ($trace-varef))
         (parameterize* (($trace-option #f))
          (print-truncated "~a : VAREF : ~s -> ~s" #f expr value)))
        value))
      ((and (pair? expr) (list? expr))
       (let ((id trace-id))
        (set! trace-id (add1 trace-id))
        (when (and ($trace-option) ($trace-start)) (print-truncated "~a : START : ~s" id expr))
        (let ((vals ($call-with-values (λ () (normal-eval expr env)) list)))
         (unless (or (not (and ($trace-option) ($trace-finis)))
                     (and ($trace-start) (= id (sub1 trace-id))))
          (print-truncated "~a : FINIS : ~s" id expr))
         (when (and ($trace-option) ($trace-value))
          (case (length vals)
           ((0) (print-truncated "~a : NOVAL" id))
           ((1) (print-truncated "~a : VALUE : ~s" id (car vals)))
           (else
            (let loop ((vals vals) (i 1))
             (unless (null? vals)
              (print-truncated "~a : MULTV : ~s : ~s" id i (car vals))
              (loop (cdr vals) (add1 i)))))))
         (apply values vals))))
      (else
       (when (and ($trace-option) ($trace-selfi))
        (parameterize* (($trace-option #f))
         (print-truncated "~a : SELFI : ~s" #f expr)))
       expr))))

   ((bool-param-guard) (λ (x) (and x #t)))

   (($trace-start)
    (register-put! 'trace-start #f (make-parameter #f bool-param-guard 'trace-start)))

   (($trace-finis)
    (register-put! 'trace-finis #f (make-parameter #f bool-param-guard 'trace-finis)))

   (($trace-value)
    (register-put! 'trace-value #f (make-parameter #f bool-param-guard 'trace-value)))

   (($trace-varef)
    (register-put! 'trace-varef #f (make-parameter #f bool-param-guard 'trace-varef)))

   (($trace-assgn)
    (register-put! 'trace-assgn #f (make-parameter #f bool-param-guard 'trace-assgn)))

   (($trace-selfi)
    (register-put! 'trace-selfi #f (make-parameter #f bool-param-guard 'trace-selfi)))

   ((trace-align-guard)
    (λ (x)
     (unless (exact-nonnegative-integer? x)
      (raise-argument-error 'trace-align "(list/c natural? natural?)" x))
     x))

   ((trace-width-guard)
    (λ (x)
     (unless (or (not x) (exact-nonnegative-integer? x))
      (raise-argument-error 'trace-width "(or/c #f natural?)" x))
     x))

   ((default-align default-width trace-id) (values 4 #f 0))

   (($trace-align)
    (register-put! 'trace-align #f
     (make-parameter default-align  trace-align-guard 'trace-align)))

   ((trace-width)
    (register-put! 'trace-width #f (make-parameter default-width trace-width-guard 'trace-width)))

   ((trace-id-align)
    (λ (id)
     (if id
      (string-append
       (let* ((str (format "~a" id)) (n (string-length str)))
        (string-append (make-string (max 0 (- ($trace-align) n)) #\space) str)))
      (make-string ($trace-align) #\space))))

   ((print-truncated)
    (λ (fmt id . rest)
     (let*
      ((m (trace-width))
       (str (apply format fmt (trace-id-align id) rest))
       (n (string-length str)))
      (printf "~a~n" (if (and m (> n m)) (string-append (substring str 0 m) " ...") str)))))

   ((@begin)
    (let
     ((begin
      (λ (uargs env)
       (cond
        ((null? uargs) (void))
        (else (begin-help (car uargs) (cdr uargs) env))))))
     (register-put! 'begin 'macro begin)
     begin))

   ((begin-help)
    (λ (kar kdr env)
     (cond
      ((null? kdr) (*eval kar env))
      (else (*eval kar env) (begin-help (car kdr) (cdr kdr) env)))))

   ((@quote)
    (register-put! 'quote 'macro
     (λ (uargs env)
      (check-quote uargs)
      (car uargs))))

   ((@quasiquote)
    (register-put! 'quasiquote 'macro
     (λ (uargs env)
      (qq (car uargs) env 0))))

   ((qq)
    (λ (x env n)
     (cond
      ((qq? x) (list qq-symbol (qq (cadr x) env (add1 n))))
      ((uq? x)
       (if (zero? n) (*eval (cadr x) env)
        (list uq-symbol (qq (cadr x) env (sub1 n)))))
      ((uqs? x)
       (let*
        ((head (car x))
         (tail (cdr x)))
        (if (zero? n)
         (let ((lst (*eval (cadar x) env)))
          (cond
           ((null? tail) lst)
           (else
            (unless (list? lst)
             (error 'unquote-splicing
              "list expected as value of: ~s~n  value: ~s"
              (cadr head) lst))
            (append lst (qq tail env n)))))
         (cons (list qq-symbol (qq (cadr head) env (sub1 n))) (qq tail env n)))))
      ((pair? x) (cons (qq (car x) env n) (qq (cdr x) env n)))
      ((vector? x)
       (let ((lst (qq (vector->list x) env n)))
        (unless (list? lst)
         (error 'quasiquote
          "incorrect unquote-splicing within vector~n  expr: ~s~n  value: ~s"
          x lst))
        (apply vector lst)))
      ((box? x) (box (qq (unbox x) env n)))
      (else x))))

   ((qq?) (λ (x) (and (list? x) (= (length x) 2) (eq? (car x) qq-symbol))))
   ((uq?) (λ (x) (and (list? x) (= (length x) 2) (eq? (car x) uq-symbol))))
   ((uqs?) (λ (x) (and (pair? x) (list? (car x)) (= (length (car x)) 2) (eq? (caar x) uqs-symbol))))
   ((qq-symbol)  'quasiquote)
   ((uq-symbol)  'unquote)
   ((uqs-symbol) 'unquote-splicing)

   ((@env)
    (register-put! 'current-env 'macro
     (λ (uargs env) env)))

   ((@lambda)
    (let*
     ((proc
       (λ (uargs env)
        (check-lambda uargs)
        (let ((formals (car uargs)) (body (cdr uargs)))
         (make-closure #f
          (λ actuals (@begin body (extend-env env formals actuals)))))))
      (macro ($make-macro 'lambda proc)))
     (register-put! 'lambda #f macro)
     (register-put! 'λ #f macro) ; Register synonym λ too.
     proc))

   ((@macro)
    (register-put! 'macro 'macro
     (λ (uargs env)
    #;(check-macro-uags uargs)
     (let ((formals (car uargs)) (body (cdr uargs)))
      (let ((proc (@lambda uargs env)))
       ($make-macro #f
        (λ (uargs caller-env)
         (*eval (@begin body (extend-env env formals uargs)) caller-env))))))))

   ((@let)
    (register-put! 'let 'macro
     (λ (uargs env)
      (cond
       ((named-let? uargs) (named-let (car uargs) (cadr uargs) (cddr uargs) env))
       ((unnamed-let? uargs) (unnamed-let (car uargs) (cdr uargs) env))
       (else (error 'let "incorrect form: ~s" (cons 'let uargs)))))))

   ((unnamed-let)
    (λ (boundlist body env)
     (let
      ((vals (evallist (map cadr boundlist) env))
       (vars (map car boundlist)))
      (@begin body (extend-env env vars vals)))))

   ((named-let)
    (λ (name boundlist body env)
     (let*
      ((proc-env (extend-env env (list name) (list $undefined)))
       (proc (@lambda (cons (map car boundlist) body) proc-env)))
      (proc-env name proc)
      (apply proc (evallist (map cadr boundlist) env)))))

   ((@let*)
    (register-put! 'let* 'macro
     (λ (uargs env)
      (check-let* uargs)
      (let*
       ((boundlist (car uargs))
        (body (cdr uargs))
        (vars (map car boundlist))
        (exprs (map cadr boundlist)))
       (let loop ((vars vars) (exprs exprs) (env env))
        (cond
         ((null? vars) (@begin body env))
         (else
          (loop (cdr vars) (cdr exprs)
           (extend-env env (list (car vars)) (list (*eval (car exprs) env)))))))))))

   ((@letrec)
    (register-put! 'letrec 'macro
     (λ (uargs env)
      (check-letrec uargs)
      (let*
       ((boundlist (car uargs))
        (body (cdr uargs))
        (vars (map car boundlist))
        (exprs (map cadr boundlist))
        (env (extend-env env vars ($make-list (length vars) $undefined))))
       (let loop ((vars vars) (exprs exprs))
        (cond
         ((null? vars) (@begin body env))
         (else
          (env (car vars) (*eval (car exprs) env))
          (loop (cdr vars) (cdr exprs)))))))))

   ((@let-values)
    (register-put! 'let-values 'macro
     (λ (uargs env)
      (check-let-values uargs)
      (let*
       ((boundlist (car uargs))
        (body (cdr uargs))
        (id-lists (map car boundlist))
        (exprs (map cadr boundlist))
        (mvals (map (λ (expr) (eval-to-list expr env)) exprs)))
       (@begin body
        (extend-env env (apply append id-lists) (apply append mvals)))))))

   ((@let*-values)
    (register-put! 'let*-values 'macro
     (λ (uargs env)
      (check-let*-values uargs)
      (let*
       ((boundlist (car uargs))
        (body (cdr uargs))
        (id-lists (map car boundlist))
        (exprs (map cadr boundlist)))
       (let loop ((id-lists id-lists) (exprs exprs) (env env))
        (cond
         ((null? id-lists) (@begin body env))
         (else
          (loop (cdr id-lists) (cdr exprs)
           (extend-env env (car id-lists)
            (eval-to-list (car exprs) env))))))))))

   ((@letrec-values)
    (register-put! 'letrec-values 'macro
     (λ (uargs env)
      (check-letrec-values uargs)
      (let*
       ((boundlist (car uargs))
        (body (cdr uargs))
        (id-lists (map car boundlist))
        (exprs (map cadr boundlist))
        (all-ids (apply append id-lists))
        (undef ($make-list (length all-ids) $undefined))
        (env (extend-env env all-ids undef)))
       (let loop ((id-lists id-lists) (exprs exprs))
        (cond
         ((null? exprs) (@begin body env))
         (else
          (for-each (λ (id val) (env id val)) (car id-lists)
           (eval-to-list (car exprs) env))
          (loop (cdr id-lists) (cdr exprs)))))))))

   ((@bindings)
    (let ((not-found (string->uninterned-symbol "not found")))
     (register-put! 'bindings 'macro
      (λ (uargs env)
      ;(check-bindings-uargs uargs)
       (let ((id (*eval (car uargs) env)))
        (unless (symbol? id)
         (error 'bindings
          "expr ~s did not produce a symbol, it yielded: ~s"
          (car uargs) id))
        (append
         (if (hash-has-key? (env-hash env) id) '(local) '())
         (if (eq? (namespace-variable-value id #t (λ () not-found) clean-environment) not-found)
          '() '(clean))
         (if (hash-has-key? (global-table-hash ($current-global-table)) id) '(global) '())))))))

   ((@let/cc)
    (register-put! 'let/cc 'macro
     (λ (uargs env)
    #;(check-let/cc-uargs)
      (call/cc
       (λ (cc) (@begin (cdr uargs) (extend-env env (list (car uargs)) (list cc))))))))

   ((@let/ec)
    (register-put! 'let/ec 'macro
     (λ (uargs env)
    #;(check-let/ec-uargs)
      (call/ec
       (λ (ec) (@begin (cdr uargs) (extend-env env (list (car uargs)) (list ec))))))))

  ((@if)
    (register-put! 'if 'macro
     (λ (uargs env)
      (check-if uargs)
      (*eval ((if (*eval (car uargs) env) cadr caddr) uargs) env))))

   ((@iflet)
    (register-put! 'iflet 'macro
     (λ (uargs env)
     ;(check-iflet uargs)
      (let*-values
       (((var test then-expr else-expr) (apply values uargs))
        ((test) (*eval test env))
        ((env) (extend-env env (list var) (list test))))
       (*eval (if test then-expr else-expr) env)))))

   ((@unless)
    (register-put! 'unless 'macro
     (λ (uargs env)
      (unless (*eval (car uargs) env) (@begin (cdr uargs) env)))))

   ((@when)
    (register-put! 'when 'macro
     (λ (uargs env)
      (when (*eval (car uargs) env) (@begin (cdr uargs) env)))))

   ((@cond)
    (register-put! 'cond 'macro
     (λ (uargs env)
      (check-cond uargs)
      (cond
       ((null? uargs) (void))
       (else
        (let loop ((clause (car uargs)) (clauses (cdr uargs)))
         (cond
          ((null? clauses)
           (cond
            ((= (length clause) 1) (*eval (car clause) env))
            ((*eval (car clause) env) (@begin (cdr clause) env))
            (else (void))))
          (else
           (let ((test (*eval (car clause) env)))
            (cond
             (test (if (= (length clause) 1) test (@begin (cdr clause) env)))
             (else (loop (car clauses) (cdr clauses)))))))))))))

   ((@case)
    (register-put! 'case 'macro
     (λ (uargs env)
      (let ((key (*eval (car uargs) env)))
       (let loop ((clauses (cdr uargs)))
        (cond
         ((null? clauses) (void))
         ((eq? (caar clauses) 'else) (@begin (cdar clauses) env))
         ((member key (caar clauses)) (@begin (cdar clauses) env))
         (else (loop (cdr clauses)))))))))

   ((@and)
    (register-put! 'and 'macro
     (λ (uargs env)
      (cond
       ((null? uargs) #t)
       (else (and-help (car uargs) (cdr uargs) env))))))

   ((and-help)
    (λ (uarg uargs env)
     (cond
      ((null? uargs) (*eval uarg env))
      ((*eval uarg env) (and-help (car uargs) (cdr uargs) env))
      (else #f))))

   ((@or)
    (register-put! 'or 'macro
     (λ (uargs env)
      (cond
       ((null? uargs) #f)
       (else (or-help (car uargs) (cdr uargs) env))))))

   ((or-help)
    (λ (uarg uargs env)
     (cond
      ((null? uargs) (*eval uarg env))
      (else
       (let ((val (*eval uarg env)))
        (cond
         (val val)
         (else (or-help (car uargs) (cdr uargs) env))))))))

   ((eval-to-list)
    (λ (uarg env)
     ($call-with-values (λ () (*eval uarg env)) list)))

   ((@parameterize)
    (register-put! 'parameterize* 'macro
     (λ (uargs env)
      (check-parameterize* uargs)
       (let*
        ((boundlist (car uargs))
         (body (cdr uargs))
         (params (map car boundlist))
         (exprs (map cadr boundlist)))
        (let loop ((params params) (exprs exprs))
         (cond
          ((null? params) (@begin body env))
          (else
           (parameterize* (((*eval (car params) env) (*eval (car exprs) env)))
            (loop (cdr params) (cdr exprs))))))))))

   ((@set!)
    (register-put! 'set! 'macro
     (λ (uargs env)
      (check-set! uargs)
      (env (car uargs) (*eval (cadr uargs) env)))))

   ((@set!-values)
    (register-put! 'set!-values 'macro
     (λ (uargs env)
      (let ((ids (car uargs)) (expr (cadr uargs)))
       (let
        ((old-vals (map env ids))
         (vals (call-with-values (λ () (*eval expr env)) list)))
        (for-each env ids vals)
        (apply values old-vals))))))

   (($reset)
    (register-put! 'reset 'proc
     (λ ()
      ($trace-option #f)
      ($trace-start #f)
      ($trace-finis #f)
      ($trace-varef #f)
      ($trace-value #f)
      ($trace-assgn #f)
      ($trace-align default-align)
      (trace-width default-width)
      (set! racket-namespace (make-racket-namespace))
      ($current-global-table ($empty-global-table))
      (void))))

   ((@with-env)
    (register-put! 'with-env 'macro
     (λ (uargs env)
      (check-with-env uargs)
      (let*
       ((body (cdr uargs))
        (local-env (*eval (car uargs) env))
        (local-env
         (cond
          ((eq? local-env #f) $empty-env)
          ((eq? local-env #t) env)
          ((eq? local-env 'copy) ($copy-env env))
          (($env? local-env) local-env)
          (else (raise-argument-error 'with-env "or/c #f #t 'copy env?" local-env)))))
       (@begin body local-env)))))

   (($eval)
    (register-put! 'eval 'proc
     (λ (expr . wrapped-env)
      (check-eval-env-arg wrapped-env)
      (let*
       ((env (if (null? wrapped-env) $empty-env (car wrapped-env))))
       (*eval expr env)))))

   ((make-racket-namespace)
    (λ ()
     (let ((namespace (make-base-namespace)))
      (namespace-set-variable-value! 'simplisp simplisp #t namespace #t)
      namespace)))

   ((racket-namespace) (make-racket-namespace))

   (($racket)
    (register-put! 'racket 'proc
     (λ exprs
      (cond
       ((null? exprs) (void))
       ((null? (cdr exprs)) (racket-eval (car exprs) racket-namespace))
       (else (racket-eval (car exprs) racket-namespace) (apply $racket (cdr exprs)))))))

   (($racket-reset)
    (register-put! 'racket-reset 'proc
     (λ () (set! racket-namespace (make-racket-namespace)))))

   (($catch-exn)
    (register-put! 'catch-exn #f
     ($make-macro 'catch-exn
      (λ (uargs env)
      ;(check-catch-exn uargs)
       (let ((catcher (*eval (car uargs) env)))
        (unless (and (procedure? catcher) (procedure-arity-includes? catcher 1))
         (raise-argument-error 'catch-exn "(procedure-arity-includes/c 1)" catcher))
        (with-handlers* ((exn:fail? catcher)) (@begin (cdr uargs) env)))))))

   ((@with-handlers*)
    (register-put! 'with-handlers* 'macro
     (λ (uargs env)
     ;(check-with-handlers* uargs)
      (let*
       ((boundlist (car uargs))
        (body (cdr uargs))
        (predicates (map car boundlist))
        (handlers (map cadr boundlist))
        (predicate/handlers
         (map (λ (binding) (*eval (car binding) env) (*eval (cadr binding) env)) boundlist)))
       (let loop ((predicate/handlers (reverse predicate/handlers)))
        (if (null? predicate/handlers) (@begin body env)
         (with-handlers* (((caar predicate/handlers) (cadar predicate/handlers)))
          (loop (cdr predicate/handlers)))))))))

   ((@trace)
    (register-put! 'trace 'macro
     (λ (uargs env)
    #;(check-trace-uargs)
      (parameterize* (($trace-option (*eval (car uargs) env))) (@begin (cdr uargs) env)))))

   ((make-global-table $global-table? global-table-hash)
    (let*-values
     (((printer)
       (λ (global-table port mode)
        (parameterize* (($trace-option #f))
         (fprintf port "#<global-table:~s>" (super-type-name global-table)))))
      ((descr constr pred acc mut)
       (make-struct-type
        'global-table ; name
        super-type    ; super-typetype
        1             ; hash, name already in super-type
        0             ; no auto fields
        #f            ; auto-value n.a.
        (list (cons prop:custom-write printer) (cons prop:object-name super-type-name))
        inspector
        #f            ; no procedure property
        '(0)          ; immutable, but the hash itself is mutable.
        #f)))         ; no guard
     (values constr
      (register-put! 'global-table? #f pred)
      (make-struct-field-accessor acc 0 'hash))))

   ((global-table-guard)
    (λ (x)
     (unless ($global-table? x)
      (raise-argument-error 'global-vars "global-vars?" x))
     x))

   (($empty-global-table)
    (register-put! 'empty-global-table 'proc
     (λ () (make-global-table #f (make-hasheq)))))

   (($current-global-table)
    (register-put! 'current-global-table #f
     (make-parameter ($empty-global-table) global-table-guard)))

   ((@global-define)
    (register-put! 'global-define 'macro
     (λ (uargs env)
      (check-global-define uargs)
      ((if (symbol? (car uargs)) global-define-id global-define-proc) uargs env))))

   ((global-define-id)
     (λ (uargs env)
      (let*
       ((id (car uargs))
        (env (extend-env env (list id) (list $undefined)))
        (val (*eval (cadr uargs) env)))
       (hash-set! (global-table-hash ($current-global-table)) id (infer-name id val))
       (env id val)
       val)))

   ((global-define-proc)
     (λ (uargs env)
      (let*
       ((id (caar uargs))
        (env (extend-env env (list id) (list $undefined)))
        (formals (cdar uargs))
        (body (cdr uargs))
        (val (@lambda (cons formals (cdr uargs)) env)))
       (hash-set! (global-table-hash ($current-global-table)) id (infer-name id val))
       (env id val)
       val)))

   ((@global-define-values)
    (register-put! 'global-define-values 'macro
     (λ (uargs env)
    #;(check-global-define-values uargs)
      (let*
       ((ids (car uargs))
        (env (extend-env env ids ($make-list (length ids) $undefined)))
        (vals (eval-to-list (cadr uargs) env))
        (h (global-table-hash ($current-global-table))))
       (for-each (λ (id val) (env id val) (hash-set! h id (infer-name id val))) ids vals)
       (apply values vals)))))

   (($global-set!)
    (register-put! 'global-set! 'macro
     (λ (uargs env)
     ;(check-global-set!)
      (let* ((id (car uargs)) (old-val (global-ref id)) (val (*eval (cadr uargs) env)))
       (hash-set! (global-table-hash ($current-global-table)) id (infer-name id val))
       old-val))))

   (($global-set!-values)
    (register-put! 'global-set!-values 'macro
     (λ (uargs env)
     ;(check-global-set!)
      (let*
       ((ids (car uargs))
        (old-vals (map global-ref ids))
        (h (global-table-hash ($current-global-table)))
        (vals (eval-to-list (cadr uargs) env)))
       (for-each (λ (id val) (hash-set! h id (infer-name id val))) ids vals)
       (apply values old-vals)))))

   ((global-ref)
    (λ (id)
     (hash-ref (global-table-hash ($current-global-table)) id
      (λ () (error 'global-ref "global var ~s not found" id)))))

   ((@global-ref)
    (register-put! 'global-ref 'macro
     (λ (uargs env)
      (check-global-ref uargs)
      (global-ref (car uargs)))))

   ((@global-ref-values)
    (register-put! 'global-ref-values 'macro
     (λ (uargs env)
    #;(check-global-ref-values uargs)
      (apply values (map global-ref uargs)))))

   (($copy-global-table)
    (register-put! 'copy-global-table 'proc
     (λ (table)
      (make-global-table #f (make-hasheq (hash-map (global-table-hash table) cons))))))

   (($global-vars)
    (register-put! 'global-vars 'proc
     (λ () (sort (hash-keys (global-table-hash ($current-global-table))) symbol<?))))

   (($global-remove!)
    (register-put! 'global-remove! #f
     ($make-macro 'global-remove!
      (λ (uargs env)
       (global-remove-help uargs (global-table-hash ($current-global-table)))))))

   ((global-remove-help)
    (λ (ids hash)
     (unless (null? ids)
      (hash-remove! hash (car ids))
      (global-remove-help (cdr ids) hash))))

   (($global-let)
    (register-put! 'global-let #f
     ($make-macro 'global-let
      (λ (uargs env)
       (let*
        ((id-list (car uargs))
         (body (cdr uargs))
         (global-ids (map (λ (x) (if (symbol? x) x (cadr x))) id-list))
         (local-ids (map (λ (x) (if (symbol? x) x (car x))) id-list))
         (vals (map global-ref global-ids)))
        (@begin body (extend-env env local-ids vals)))))))

   ((quote-uargs?) (λ (x) (= (length x) 1)))
   ((lambda-uargs?) (λ (x) (and (pair? x) (formals? (car x)))))
   ((named-let?) (λ (x) (and (pair? x) (symbol? (car x)) (bound-list? (cadr x)))))
   ((unnamed-let?) (λ (x) (and (pair? x) (bound-list? (car x)))))
   ((let-values-uargs?) (λ (x) (and (pair? x) (values-bound-list? (car x)))))
   ((values-bound-list?) (λ (x) (and (list? x) (andmap m-binding? x))))
   ((parameterize*-uargs?) (λ (x) (and (pair? x) (list? (car x)) (andmap list2? (car x)))))
   ((set!-uargs?) (λ (x) (and (= (length x) 2) (symbol? (car x)))))
   ((with-env-uargs?) void)
   ((null-form-uargs?) (λ (x) (and (= (length x) 1) (symbol? (car x)))))
   ((bound-list?) (λ (x) (and (list? x) (andmap binding? x))))
   ((binding?) (λ (x) (and (list? x) (= (length x) 2) (symbol? (car x)))))
   ((define-proc?) (λ (x) (and (pair? x) (pair? (car x)) (formals? (car x)))))
   ((list2?) (λ (x) (and (list? x) (= (length x) 2))))
   ((m-binding?)(λ(x)(and(list? x)(=(length x)2)(list?(car x))(andmap symbol?(car x)))))
   ((formals?) (λ(x) (or (null? x) (symbol? x) (and (pair? x)(symbol?(car x))(formals?(cdr x))))))

   ((make-check)
    (λ (name pred)
     (λ (x)
      (unless (pred x)
       (error name "incorrect form: ~s" (cons name x))))))

   ((check-macro) void)
   ((check-quote)         (make-check 'quote quote-uargs?))
   ((check-lambda)        (make-check 'lambda lambda-uargs?))
   ((check-let*)          (make-check 'let* unnamed-let?))
   ((check-letrec)        (make-check 'letrec unnamed-let?))
   ((check-let-values)    (make-check 'let-values let-values-uargs?))
   ((check-let*-values)   (make-check 'let*-values let-values-uargs?))
   ((check-letrec-values) (make-check 'letrec-values let-values-uargs?))
   ((check-if)            (make-check 'if (λ (x) (= (length x) 3))))
   ((check-cond)          (make-check 'cond (λ (x) (andmap list? x))))
   ((check-parameterize*) (make-check 'parameterize* parameterize*-uargs?))
   ((check-set!)          (make-check 'set! set!-uargs?))
   ((check-with-env)      (make-check 'with-env with-env-uargs?))
   ((check-null-form)     (make-check 'null-form null-form-uargs?))
   ((check-eval-env-arg) void)
   ((check-global-ref)    (make-check 'global-vars-ref void))
   ((check-global-define) (make-check 'global-vars-set! void))

   ((special-var-names) (fill-clean-environment!)))

 simplisp))

#| The end.

aap noot mies wim zus jet teun vuur gijs lam kees bok weide does hok duif schapen |#
