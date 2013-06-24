;; racket-rewrites.el --- part of sexp-rewrite package

;; Copyright 2013 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later.
;; See the top of the sexp-rewrite.el file for details.

;; ============================================================
;; TO DO

;; case-lambda-sort-clauses
;; case-lambda-to-lambda-with-optional-arguments

;; short term
;; - build big library of Scheme/Racket tactics
;; - build automatic tactics
;;   - general purpose expressions
;;   - coalescing conditionals
;;   - working in definition contexts
;;   - ...?
;; - find polite way to set Racket tactics vars only in "Racket mode"

;; long term
;; - port to DrRacket, etc (see sexp-rewrite todo)

;; ============================================================
;; On safety

;; Need to figure out how to compromise between safety and usability.

;; Every last one of these is unsafe if literals don't have their
;; standard bindings.
;; 
;; Most of them are slightly unsafe.

;; ============================================================
;; Using racket-rewrites

;; Most of the tactics below have strings following the definition
;; that let you see the effect of the tactic.

;; Once you've tried the tactic, press "C-x u" to undo the change and
;; restore the example.

;; ============================================================
;; Expression rewrites
;; Could be triggered automatically via pattern search.

(setq sexprw-auto-expression-tactics
      (append '(if-to-cond
                cond-else-absorb-cond
                cond-else-absorb-if
                let-if-to-cond
                cond-else-absorb-let-if
                letrec-to-definitions
                let-loop-to-definition
                ;; let-to-definitions
                begin-trivial
                map for-each ormap andmap foldl
                build-list for/sum-from-map for/sum-from-for/list
                in-list-vector->list)
              sexprw-auto-expression-tactics))

(define-sexprw-tactic if-to-cond
  (sexprw-rewrite
   '(if %test %then %else)
   '(cond (!SQ %test %then) !NL
          (!SQ else %else))))

';; example for if-to-cond, cond-else-absorb-*
(if (< x 10)
    (f x)
    (if (> x 10)
        (g x)
        (let ((y (h x)))
          (if y
              (k y)
              0))))

(define-sexprw-tactic cond-else-absorb-cond
  (sexprw-rewrite
   '(cond %%clauses (else (cond %%more-clauses)))
   '(cond %%clauses !NL %%more-clauses)))

(define-sexprw-tactic cond-else-absorb-if
  (sexprw-rewrite
   '(cond %%clauses (else (if %test %then %else)))
   '(cond %%clauses !NL (!SQ %test %then) !NL (!SQ else %else))))

(define-sexprw-tactic let-if-to-cond
  ;; Unsafe if $name occurs free in %else
  (sexprw-rewrite
   '(let (($name %rhs))
      (if $name %then %else))
   '(cond (!SQ %rhs !NL => (lambda ($name) !NL %then)) !NL
          (!SQ else !NL %else))))

(define-sexprw-tactic cond-else-absorb-let-if
  ;; Unsafe if $name occurs free in %else
  (sexprw-rewrite
   '(cond %%rest (else (let (($name %rhs)) (if $name %then %else))))
   '(cond %%rest !NL
          (!SQ %rhs !NL => (lambda ($name) !NL %then)) !NL
          (!SQ else %else))))

';; example for let-if-to-cond
(let ([x (assq key alist)])
  (if x
      (cdr x)
      (error 'no-key)))

';; negative example for unsafe-let-if-to-cond
(let ((x (assq key alist)))
  (if different-var
      (cdr x)
      (error 'no-key)))

(define-sexprw-tactic letrec-to-definitions
  (sexprw-rewrite
   '(letrec (($name %rhs) ...) %%body)
   '(let () !NL (!SPLICE (define $name %rhs) !NL) ... %%body)))

';; example for letrec-to-definitions
(letrec ([odd? (lambda (x) (not (even? x)))]
         [even? (lambda (x) (or (zero? x) (even? (sub1 x))))])
  odd?)

(define-sexprw-tactic let-loop-to-definition
  ;; Unsafe if $name occurs free in %init
  (sexprw-rewrite
   '(let $loop (($arg %init) ...) %%body)
   '(let () !NL
      (define ($loop $arg ...) !NL
        %%body)
      !NL
      ($loop %init ...))))

;; Would be nice to recognize potential 'for' loops,
;; but needs a lot more information than we have here.

';; example for let loop
(let loop ([rejected 0] [racc '()] [lst the-stuff])
  (cond [(pair? lst)
         (if (ok? (car lst))
             (loop count (cons (car lst) racc) (cdr lst))
             (loop (add1 count) racc (cdr lst)))]
        [else
         (values rejected (reverse racc))]))

(define-sexprw-tactic let-to-definitions
  ;; Unsafe if any %rhs has free occurrences of any $name,
  ;; or if %%body contains definitions of some $x where $x collides with
  ;; some $name or if $x occurs free in any %rhs.
  (sexprw-rewrite
   '(let (($name %rhs) ...) %%body)
   '(let () !NL (!SPLICE (define $name %rhs) !NL) ... %%body)))

(define-sexprw-tactic begin-trivial
  ;; See also let-splice, begin-splice
  (sexprw-rewrite
   '(begin %body)
   '%body))

';; example for begin-trivial
(begin 5)

;; HO functions to for loops

(define-sexprw-tactic map (r-*map 'map 'for/list))
(define-sexprw-tactic for-each (r-*map 'for-each 'for))
(define-sexprw-tactic ormap (r-*map 'ormap 'for/or))
(define-sexprw-tactic andmap (r-*map 'map 'for/and))

(defun r-*map (map-symbol for-symbol)
  (sexprw-rewrite
   `(,map-symbol (lambda ($arg ...) %%body) %lst ...)
   `(,for-symbol ((!SPLICE (!SQ $arg (in-list %lst)) !NL) ...) !NL %%body)))

';;example for ormap
(define (pointwise< xs ys)
  (not (ormap (lambda (x y) (>= x y)) xs ys)))

';;example for map
(define (frobble xs ys)
  (map (lambda (x y) (bargle x y)) xs ys))

;; TODO: 
;;      (map (lambda (e) body) (vector->list v))
;;   => (for/list ([e (in-vector v)]) body)
;; or instead, just
;;      (in-list (vector->list v)) => (in-vector v)
;; Perhaps group tactics like (in-list (vector->list _)) together
;; under tactic name, use recursive processing to apply?
;; (Don't want to automatically try *all* expr tactics, probably.)
;; Problem: not recursively processing original text, but generated text.

(define-sexprw-tactic foldl
  (sexprw-rewrite
   '(foldl (lambda ($arg ... $accum) %%body) %init %lst ...)
   '(for/fold ((!SQ $accum %init)) !NL
              ((!SPLICE (!SQ $arg (in-list %lst)) !NL) ...) !NL
      %%body)))

';; example for foldl
(foldl (lambda (x y acc) (f x (g y acc) acc))
       0
       some-things
       (append better-things worse-things))

;; What about for-loop fusion or absorption?
;; (for/* ([$name (in-list (filter %pred %lst))]) %%body)
;; => (for/* ([$name (in-list $lst)]
;;            #:when (%pred $name))    ;; unsafe: puts %pred in scope of $name
;;      %%body)

(define-sexprw-tactic build-list
  (sexprw-rewrite
   '(build-list %n (lambda ($arg) %%body))
   '(for/list ((!SQ $arg (in-range %n))) !NL %%body)))

(define-sexprw-tactic for/sum-from-map
  (sexprw-rewrite
   '(apply + (map (lambda ($arg ...) %%body) %lst ...))
   '(for/sum ((!SPLICE (!SQ $arg (in-list %lst)) !NL) ...) !NL %%body)))

(define-sexprw-tactic for/sum-from-for/list
  (sexprw-rewrite
   '(apply + (for/list %%body))
   '(for/sum %%body)))

(define-sexprw-tactic in-list-vector->list
  (sexprw-rewrite
   '(in-list (vector->list %e))
   '(in-vector %e)))

(define-sexprw-tactic case-lambda-sort-clauses
  (sexprw-rewrite
   '(case-lambda (($var ...) %%body) ...)
   '(case-lambda !NL (!SPLICE (!SQ ($sorted-var ...) !NL %%sorted-body) !NL) ...)
   (lambda (env)
     ;; check no $var is a dot (means rest args)
     ;; sort vars and bodies together by length of vars list
     (cond ((let (found-dot)
              (dolist (vars-entry (cdr (cdr (assq '$var env))))
                (dolist (var (cdr vars-entry))
                  (when (equal var '(atom "."))
                    (setq found-dot t))))
              found-dot)
            nil)
           (t
            (let ((clauses nil)
                  (var-entries (cdr (cdr (assq '$var env))))
                  (bodies (cdr (cdr (assq '%%body env)))))
              (while var-entries
                (setq clauses (cons (cons (car var-entries) (car bodies)) clauses))
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
              (list
               (append
                (list (cons '$sorted-var (cons 'rep var-entries))
                      (cons '%%sorted-body (cons 'rep bodies)))
                env))))))))

';; example for case-lambda
(define f
  (case-lambda
   [(x) (f x 2])
   [(x y) (f x y)]
   [() (f 1)]))

(define-sexprw-tactic define-case-lambda-to-optionals
  '(define $name
     (case-lambda
      (($arg ...) ($uname $arg ... %newarg)) ...
      (($farg ...) %%body)))
  '(define ($name $required-arg ... (!SQ $optional-arg %newarg) ...) !NL
     %%body)
  (lambda (env)
    ;; each $uname is $name (nonlinear pvars don't work here, different depths :(
    ;; each arglist is one shorter than next ($arg names don't have to match)
    ;; done by pattern: each clause applies $uname to args, plus one new arg at end
    ;; split $farg into $required-arg and $optional-arg
    nil))



;; ============================================================
;; Definition rewritings

;; Most of these are unsafe if applied in an expression context.

(setq sexprw-auto-definition-tactics
      (append '(define-absorb-lambda
                splice-begin
                splice-letrec
                splice-empty-let)
              sexprw-auto-definition-tactics))

(define-sexprw-tactic define-absorb-lambda
  (sexprw-rewrite
   '(define $name (lambda ($arg ...) %%body))
   '(define ($name $arg ...) !NL %%body)))

';; example for define-absorb-lambda
(define x
  (lambda (y)
    ;; a comment
    (displayln y)
    (+ y 1)
    ;; trailing comment
    ))

(define-sexprw-tactic splice-begin
  (sexprw-rewrite
   '(begin %%body)
   '%%body))

(define-sexprw-tactic splice-letrec
  ;; Unsafe, changes scope of $names
  (sexprw-rewrite
   '(letrec ((!REP ($name %rhs))) %%body)
   '(!SPLICE (!REP (define $name !NL %rhs) !NL) %%body)))

(define-sexprw-tactic splice-empty-let
  ;; Unsafe if %%body contains definitions: changes their scopes
  (sexprw-rewrite
   '(let () %%body)
   '%%body))

;; ============================================================
;; Specialized rewritings
;; Need to be explicitly triggered.

(define-sexprw-tactic define-split-lambda
  ;; Inverse of r-define-absorb-lambda
  (sexprw-rewrite
   '(define ($name (!REP $arg)) %%body)
   '(define $name !NL (lambda ((!REP ($arg) $arg)) !NL %%body))))

(define-sexprw-tactic eta-expand
  (sexprw-rewrite
   '%expr
   '(lambda ($arg ...) %expr)
   (lambda (env)
     (message "Running guard!")
     (let ((argn (read-number "Number of arguments: " 1)))
       (message "got number %S" argn)
       (unless (and (integerp argn) (>= argn 0))
         (error "Bad number of arguments: %S" argn))
       (message "number is good")
       (let ((args
              (cond ((= argn 0)
                     nil)
                    ((= argn 1)
                     (list (list 'atom "x")))
                    (t
                     (let ((rargs nil))
                       (message "about to loop")
                       (dotimes (i argn)
                         (message "loop i = %S" i)
                         (setq rargs (cons (list 'atom (format "x%d" (1+ i))) rargs)))
                       (reverse rargs))))))
         (message "args = %S" args)
         (list (cons (cons '$arg (cons 'rep args)) env)))))))

';; example for eta-expand
add1
