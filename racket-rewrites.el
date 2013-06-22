;; racket-rewrites.el --- part of sexp-rewrite package

;; Copyright 2013 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later.
;; See the top of the sexp-rewrite.el file for details.

;; ============================================================
;; TO DO

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
;; Using racket-rewrites

;; Most of the tactics below have strings following the definition
;; that let you see the effect of the tactic.

;; Once you've tried the tactic, press "C-x u" to undo the change and
;; restore the example.

;; ============================================================
;; Expression rewrites
;; Could be triggered automatically via pattern search.

(setq sexprw-auto-expression-tactics
      (append '(letrec-to-definitions
                begin-trivial
                if-to-cond
                cond-else-absorb-if
                cond-else-absorb-cond
                map for-each ormap andmap foldl)
              sexprw-auto-expression-tactics))

(setq sexprw-auto-conditional-tactics
      (append '(if-to-cond
                cond-else-absorb-if
                cond-else-absorb-cond
                unsafe-let-if-to-cond
                unsafe-cond-else-absorb-let-if)
              sexprw-auto-conditional-tactics))

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

;; FIXME: need to check equality for non-linear pattern

(define-sexprw-tactic unsafe-let-if-to-cond
  ;; Unsafe if $name occurs free in %else
  (sexprw-rewrite
   '(let (($name %rhs))
      (if $name %then %else))
   '(cond (!SQ %rhs !NL => (lambda ($name) !NL %then)) !NL
          (!SQ else !NL %else))))

(define-sexprw-tactic unsafe-cond-else-absorb-let-if
  ;; Unsafe if $name occurs free in %else
  (sexprw-rewrite
   '(cond %%rest (else (let (($name %rhs)) (if $name %then %else))))
   '(cond %%rest !NL
          (!SQ %rhs !NL => (lambda ($name) !NL %then)) !NL
          (!SQ else %else))))

';; example for unsafe-let-if-to-cond
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

;; ============================================================
;; More expression rewrites
;; The following rules probably aren't useful often

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

;; ============================================================
;; Definition rewritings

(setq sexprw-auto-definition-tactics
      (append '(define-absorb-lambda
                splice-let
                splice-begin)
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

;; The following are useful in definition context, unsafe in expr context

(define-sexprw-tactic splice-letrec
  (sexprw-rewrite
   '(letrec ((!REP ($name %rhs))) %%body)
   '(!SPLICE (!REP (define $name !NL %rhs) !NL) %%body)))

(define-sexprw-tactic splice-let
  (sexprw-rewrite
   '(let () %%body)
   '%%body))

(define-sexprw-tactic splice-begin
  (sexprw-rewrite
   '(begin %%body)
   '%%body))

;; ============================================================
;; Specialized rewritings
;; Need to be explicitly triggered.

(define-sexprw-tactic define-split-lambda
  ;; Inverse of r-define-absorb-lambda
  (sexprw-rewrite
   '(define ($name (!REP $arg)) %%body)
   '(define $name !NL (lambda ((!REP ($arg) $arg)) !NL %%body))))

;; TODO: eta
;;    expr => (lambda (x ...) (expr x ...))
;; Needs input from user.
