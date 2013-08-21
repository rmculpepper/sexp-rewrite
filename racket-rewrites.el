;;; racket-rewrites.el --- part of sexp-rewrite package

;;; Copyright 2013 Ryan Culpepper.
;;; Released under the terms of the GPL version 3 or later.
;;; See the top of the sexp-rewrite.el file for details.

;;; ============================================================
;;; TO DO

;;; short term
;;; - build big library of Scheme/Racket tactics
;;; - build automatic tactics
;;;   - general purpose expressions
;;;   - coalescing conditionals
;;;   - working in definition contexts
;;;   - ...?
;;; - find polite way to set Racket tactics vars only in "Racket mode"

;;; long term
;;; - port to DrRacket, etc (see sexp-rewrite todo)

;;; ============================================================
;;; On safety

;;; Need to figure out how to compromise between safety and usability.

;;; Every last one of these is unsafe if literals don't have their
;;; standard bindings.
;;; 
;;; Most of them are slightly unsafe.

;;; ============================================================
;;; Using racket-rewrites

;;; Most of the tactics below have strings following the definition
;;; that let you see the effect of the tactic.

;;; Once you've tried the tactic, press "C-/" to undo the change and
;;; restore the example.

;;; ============================================================
;;; Expression rewrites
;;; Could be triggered automatically via pattern search.

(setq sexprw-auto-expression-tactics
      '(if-to-cond
        cond-else-absorb-cond
        cond-else-absorb-if
        let-if-to-cond
        cond-else-absorb-let-if
        letrec-to-definitions
        let-loop-to-definition
        ;; let-to-definitions
        let-absorb-let*
        let*-absorb-let
        begin-trivial
        map for-each ormap andmap foldl
        build-list for/sum-from-map for/sum-from-for/list
        in-list-vector->list))

(define-sexprw-tactic if-to-cond
  (if $test $then $else)
  (cond [$test $then] !NL
        [else $else]))

' ; example for if-to-cond, cond-else-absorb-*
(if (< x 10)
    (f x)
    (if (> x 10)
        (g x)
        (let ((y (h x)))
          (if y
              (k y)
              (+ 0
                 1)))))

(define-sexprw-tactic cond-else-absorb-cond
  (cond $clauses:rest1 [else (cond $more:rest)])
  (cond $clauses !NL $more))

(define-sexprw-tactic cond-else-absorb-if
  (cond $clauses:rest1 [else (if $test $then $else)])
  (cond $clauses !NL [$test !SL $then] !NL [else !SL $else]))

(define-sexprw-tactic let-if-to-cond
  ;; Unsafe if $name occurs free in $else
  (let ([$name:id $rhs])
    (if $name:id $then $else))
  (cond [$rhs !SL => (lambda ($name) !SL $then)] !NL
        [else !SL $else]))

(define-sexprw-tactic cond-else-absorb-let-if
  ;; Unsafe if $name occurs free in %else
  (cond $clauses:rest1
        [else (let ([$name:id $rhs]) (if $name:id $then $else))])
  (cond $clauses !NL
        [$rhs !SL => (lambda ($name) !SL $then)] !NL
        [else !SL $else]))

' ; example for let-if-to-cond
(let ([x (assq key alist)])
  (if x
      (cdr x)
      (error 'no-key)))

' ; negative example for let-if-to-cond
(let ((x (assq key alist)))
  (if different-var
      (cdr x)
      (error 'no-key)))

(define-sexprw-nt let-clause
  :attributes ($def)
  (pattern [$name:id (lambda ($arg ...) $body:rest)]
           :with $def (define ($name $arg ...) !SL $body))
  (pattern [$name:id $rhs]
           :with $def (define $name !SL $rhs)))

(define-sexprw-tactic letrec-to-definitions
  (letrec ($c:let-clause ...) $body:rest)
  (let () !NL (!@ $c.$def !NL) ... $body))

' ; example for letrec-to-definitions
(letrec ([odd? (lambda (x) (not (even? x)))]
         [even? (lambda (x) (or (zero? x) (even? (sub1 x))))])
  odd?)
  
' ; another example for letrec-to-definitions
(letrec ([odd? (lambda (x) (not (even? x)))]
         [even? (lambda (x)
                  (or (zero? x)
                      (even? (sub1 x))))])
  odd?)

(define-sexprw-tactic let-to-definitions
  ;; Unsafe if any %rhs has free occurrences of any $name, or if %%body
  ;; contains definitions of some $x where $x collides with some $name
  ;; or if $x occurs free in any %rhs.
  (let ($c:let-clause ...) $body:rest)
  (let () !NL (!@ $c.$def !NL) ... $body))

' ; example for let-to-definitions
(let ((x 1) (y 2)) (+ x y))

(define-sexprw-tactic let-loop-to-definition
  ;; Unsafe if $name occurs free in %init
  (let $loop:id (($arg:id $init) ...) $body:rest)
  (let () !NL
    (define ($loop $arg ...) !SL
      $body)
    !NL
    ($loop $init ...)))

;; Would be nice to recognize potential 'for' loops,
;; but needs a lot more information than we have here.

' ; example for let-loop-to-definition
(let loop ([rejected 0] [racc '()] [lst the-stuff])
  (cond [(pair? lst)
         (if (ok? (car lst))
             (loop count (cons (car lst) racc) (cdr lst))
             (loop (add1 count) racc (cdr lst)))]
        [else
         (values rejected (reverse racc))]))

;; let/let* absorption requires single-clause lets; unsafe otherwise
;; (changes scoping)
(define-sexprw-tactic let-absorb-let*
  (let ([$var:id $rhs]) (let* ($clauses:rest) $body:rest))
  (let* ([$var $rhs] !NL $clauses) !NL $body))
(define-sexprw-tactic let*-absorb-let
  (let* ($clauses:rest) (let ([$var:id $rhs]) $body:rest))
  (let* ($clauses !NL [$var $rhs]) !NL $body))

' ; example for let-absorb-let*, let*-absorb-let
(let ((x 1))
  (let* ((y (f x))
         (z (g x y)))
    (let ((q (+ x y z)))
      (h x y z))))

(define-sexprw-tactic begin-trivial
  ;; See also let-splice, begin-splice
  (begin $body)
  $body)

' ; example for begin-trivial
(begin 5)

;; HO functions to for loops

(define-sexprw-nt list-expr
  :attributes ($for-rhs)
  (pattern (vector->list $e)
           :with $for-rhs (in-vector $e))
  (pattern (string->list $e)
           :with $for-rhs (in-string $e))
  (pattern $e
           :with $for-rhs (in-list $e)))

(defmacro define-sexprw-*map-tactic (name map-sym for-sym)
  `(define-sexprw-tactic ,name
     (,map-sym (lambda ($arg:id ...) $body:rest) $lst:list-expr ...)
     (,for-sym ((!@ [$arg $lst.$for-rhs] !NL) ...) !NL $body)))

(define-sexprw-*map-tactic map      map      for/list)
(define-sexprw-*map-tactic for-each for-each for)
(define-sexprw-*map-tactic ormap    ormap    for/or)
(define-sexprw-*map-tactic andmap   andmap   for/and)

' ; example for ormap
(define (pointwise< xs ys)
  (not (ormap (lambda (x y) (>= x y)) xs ys)))

' ; example for map
(define (frobble xs ys)
  (map (lambda (x y) (bargle x y)) xs ys))

' ; example of "optimization"
(map (lambda (e) (add1 e)) (vector->list es))

(define-sexprw-tactic foldl
  (foldl (lambda ($arg:id ... $accum:id) $body:rest) $init $lst ...)
  (for/fold ([$accum $init]) !NL
            ((!@ [$arg (in-list $lst)] !NL) ...) !NL
    $body))

' ; example for foldl
(foldl (lambda (x y acc) (f x (g y acc) acc))
       0
       some-things
       (append better-things worse-things))

;; What about for-loop fusion or absorption?
;; (for/* ([$name (in-list (filter %pred %lst))]) %%body)
;; => (for/* ([$name (in-list $lst)]
;;            #:when (%pred $name))    ; unsafe: puts %pred in scope of $name
;;      %%body)

(define-sexprw-tactic build-list
  (build-list $n (lambda ($arg:id) $body:rest))
  (for/list ([$arg (in-range $n)]) !NL $body))

(define-sexprw-tactic for/sum-from-map
  (apply + (map (lambda ($arg:id ...) $body:rest) $lst ...))
  (for/sum ((!@ [$arg:id (in-list $lst)] !NL) ...) !NL $body))

(define-sexprw-tactic for/sum-from-for/list
  (apply + (for/list $body:rest))
  (for/sum $body))

(define-sexprw-tactic in-list-vector->list
  (in-list (vector->list $e))
  (in-vector $e))

;; ============================================================
;; Definition rewritings

;; Most of these are unsafe if applied in an expression context.

(setq sexprw-auto-definition-tactics
      '(define-absorb-lambda
         splice-begin
         splice-letrec
         splice-empty-let
         define-case-lambda-sort-clauses
         define-case-lambda-to-optionals
         define-rest-to-optional))

(define-sexprw-tactic define-absorb-lambda
  (define $name:id (lambda ($arg ...) $body:rest))
  (define ($name $arg ...) !NL $body))

' ; example for define-absorb-lambda
(define f
  (lambda (x y [z (+ x y)])
    ;; a comment
    (displayln y)
    (+ y 1)
    ;; trailing comment
    ))

(define-sexprw-tactic define-absorb-lambda/curry
  (define $header (lambda ($arg ...) $body:rest))
  (define ($header $arg ...) !NL $body))

(define-sexprw-tactic splice-begin
  (begin $body:rest)
  $body)

(define-sexprw-tactic splice-letrec
  ;; Unsafe, changes scope of $names
  (letrec (($name:id $rhs) ...) $body:rest)
  (!@ (!@ (define $name !NL $rhs) !NL) ... $body))

(define-sexprw-tactic splice-empty-let
  ;; Unsafe if %%body contains definitions: changes their scopes
  (let () $body:rest)
  $body)

;; FIXME: would be better to have a case-lambda transformation, reuse
;; for define rhs
(define-sexprw-tactic case-lambda-sort-clauses
  (case-lambda (($var:id ...) $body:rest) ...)
  :guard
  (lambda (env)
    ;; check no $var is a dot (means rest args)
    ;; sort vars and bodies together by length of vars list
    (cond ((not (sexprw-guard-no-dot env '$var))
           nil)
          ;; If already sorted, tactic does not apply (else gets stuck
          ;; repeating this)
          ((let ((var-entries (cdr (sexprw-env-ref env '$var)))
                 (sorted t))
             (while (and (consp var-entries) (consp (cdr var-entries)))
               (unless (< (length (car var-entries))
                          (length (cadr var-entries)))
                 (setq sorted nil))
               (setq var-entries (cdr var-entries)))
             sorted)
           ;; (message "clauses already sorted")
           nil)
          (t
           (let ((clauses nil)
                 (var-entries (cdr (sexprw-env-ref env '$var)))
                 (bodies (cdr (sexprw-env-ref env '$body))))
             (while var-entries
               (setq clauses (cons (cons (car var-entries) (car bodies))
                                   clauses))
               (setq var-entries (cdr var-entries))
               (setq bodies (cdr bodies)))
             (setq clauses
                   (sort clauses
                         (lambda (a b)
                           ;; use >, then reverse-split result
                           (> (length (car a)) (length (car b))))))
             (dolist (clause clauses)
               (setq var-entries (cons (car clause) var-entries))
               (setq bodies (cons (cdr clause) bodies)))
             (list `(($sorted-var rep ,@var-entries)
                     ($sorted-body rep ,@bodies)
                     ,@env))))))
  ;; template:
  (case-lambda !NL
    (!@ [($sorted-var ...) !NL $sorted-body] !NL) ...))

(define-sexprw-tactic define-case-lambda-sort-clauses
  (define $name:id $body:case-lambda-sort-clauses)
  (define $name !NL $body.$out))

(define-sexprw-tactic define-case-lambda-to-optionals
  (define $name:id
    (case-lambda
     (($arg:id ...) ($uname:id $arg:id ... $newarg)) ...
     (($farg:id ...) $body:rest)))
  :guard
  (lambda (env)
    (let ((failed nil)
          (required-arg-count nil)
          ($name-entry (sexprw-env-ref env '$name))
          ($uname-entries (cdr (sexprw-env-ref env '$uname)))
          ($args-entries (cdr (sexprw-env-ref env '$arg)))
          ($fargs-entry (sexprw-env-ref env '$farg)))
      ;; each $uname is $name (nonlinear pvars don't work here,
      ;; different depths :(
      (dolist ($uname-entry $uname-entries)
        (unless (sexprw-entry-equal $uname-entry $name-entry)
          ;; (message "$uname %S failed to match $name %S"
          ;;          $uname-entry $name-entry)
          (sexprw-fail `(...-to-optionals name/uname uname-entry= ,uname-entry name-entry= ,name-entry))
          (setq failed t)))
      ;; each arglist is one shorter than next ($arg names don't have
      ;; to match)
      (let ((all-arg-entries `(,@$args-entries ,$fargs-entry)))
        ;; 1- for 'rep header
        (setq required-arg-count (1- (length (car all-arg-entries))))
        (while (and (consp all-arg-entries) (consp (cdr all-arg-entries)))
          (let ((arg-entries1 (car all-arg-entries))
                (arg-entries2 (cadr all-arg-entries)))
            (setq all-arg-entries (cdr all-arg-entries))
            (unless (= (1+ (length arg-entries1)) (length arg-entries2))
              ;; (message "bad arg lengths")
              (setq failed t)))))
      ;; done by pattern: each clause applies $uname to args, plus one
      ;; new arg at end
      ;; split $farg into $required-arg and $optional-arg
      (let ((farg-entries (cdr $fargs-entry))
            (r-required-args nil))
        (dotimes (_i required-arg-count)
          (setq r-required-args (cons (car farg-entries) r-required-args))
          (setq farg-entries (cdr farg-entries)))
        (if failed
            nil
          (list `(($required-arg rep ,@(reverse r-required-args))
                  ($optional-arg rep ,@farg-entries)
                  ,@env))))))
  ;; template:
  (define ($name $required-arg ... (!SQ $optional-arg $newarg) ...) !NL
    $body))

' ; example for define-case-lambda-sort-clauses and
  ; define-case-lambda-to-optionals
(define f
  (case-lambda
    [(x) (f x 2)]
    [() (f 1)]
    [(x y) (f x y)]))

(define-sexprw-tactic define-rest-to-optional
  ;; Unsafe if $rest used elsewhere in $body
  ;; Also, see guard
  (define ($name:id $arg:id ... \. $rest:id)
    (let (($optional-arg:id (if (null? $rest:id) $default (car $rest:id))))
      $body:rest))
  :guard
  (lambda (env)
    ;; If $default = $rest, rewrite to null
    ;; Unsafe if $default *contains* $rest
    (if (sexprw-entry-equal (sexprw-env-ref env '$default)
                            (sexprw-env-ref env '$rest))
        (list (cons (cons '$default (sexprw-template 'null env)) env))
      (list env)))
  (define ($name $arg ... [$optional-arg $default]) !NL $body))

' ; example for define-rest-to-optionals (from SXML)
(define (ddo:ancestor test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (do-stuff-with test-pred? num-anc)))

;; ============================================================
;; Specialized rewritings
;; Need to be explicitly triggered.

(define-sexprw-tactic split-let
  (let ($clause $more-clauses:rest) $body:rest)
  (let ($clause) !NL (let ($more-clauses) !NL $body)))

(define-sexprw-tactic split-let*
  ;; Occasionally useful for eg define-rest-to-optionals
  (let* ($clause $more-clauses:rest) $body:rest)
  (let ($clause) !NL (let* ($more-clauses) !NL $body)))

' ; example for split-let*
(define (blah . rest)
  (let* ((rest (if (null? rest) 0 (car rest)))
         (more (add1 rest)))
    (body)))

(define-sexprw-nt define-like-kw
  (pattern define)
  (pattern define-syntax))

(define-sexprw-tactic define-split-lambda
  ;; Inverse of define-absorb-lambda
  ($define:define-like-kw ($name:id $arg ...) $body:rest)
  ($define $name !NL (lambda ($arg ...) !NL $body)))

' ; example for define-split-lambda
(define (f x) (or x 1))

' ; another example for define-split-lambda
(define-syntax (f x) (or x 1))

;; ----

(define-sexprw-tactic beta-to-let
  ((lambda ($arg:id ...) $body:rest) $val ...)
  (let ((!@ [$arg $val] !SL) ...) !SL $body))

' ;; example for beta-to-let
((lambda (x)
   (+ x 1))
 (- 13 1))

(define-sexprw-tactic eta-reduce
  ;; Unsafe if $e has side-effects or may not terminate
  (lambda ($arg ...) ($e $arg ...))
  $e)

' ; example for eta-reduce
(lambda (x y z) (f x y z))

;; (define-sexprw-tactic eta-expand
;;   $expr
;;   :guard
;;   (lambda (env)
;;     (let ((argn (read-number "Number of arguments: " 1)))
;;       (unless (and (integerp argn) (>= argn 0))
;;         (error "Bad number of arguments: %S" argn))
;;       (let ((args
;;              (cond ((= argn 0)
;;                     nil)
;;                    ((= argn 1)
;;                     (list (sexprw-template 'x nil)))
;;                    (t
;;                     (let ((rargs nil))
;;                       (dotimes (i argn)
;;                         (push (sexprw-template (intern (format "x%d" (1+ i))) nil)
;;                               rargs))
;;                       (reverse rargs))))))
;;         (list (cons (cons '$arg (cons 'rep args)) env)))))
;;   ;; template
;;   (lambda ($arg ...) !SL $expr))

;; ' ; example for eta-expand
;; add1

;; ' ; another example for eta-expand
;; (compose foo
;;          bar)
