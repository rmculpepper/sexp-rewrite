;;; sexprw-for-clojure.el --- tactics for Clojure  -*- lexical-binding:t -*-

;; Copyright 2019 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later;
;; see sexprw.el for details.

(require 'sexprw)

;; ============================================================
;; Threading

(define-sexprw-nt thread1able-fun
  :attributes ($e $threads)
  (pattern ($f:id $e)
           :guard (lambda (env)
                    (and (not (sexprw-output-equal
                               (sexprw-template '$f env)
                               (sexprw-template '-> nil)))
                         (list env)))
           :with $threads $f)
  (pattern ($f $e $args:rest)
           :guard (lambda (env)
                    (and (not (sexprw-output-equal
                               (sexprw-template '$f env)
                               (sexprw-template '-> nil)))
                         (list env)))
           :with $threads ($f $args)))

(define-sexprw-nt thread1able
  :attributes ($threads)
  (pattern $x:thread1able-fun
           :with $threads (!@ $x.$e !NL $x.$threads))
  (pattern (-> $x:thread1able-fun $th:rest)
           :with $threads (!@ $x.$e !NL $x.$threads !NL $th))
  (pattern (-> (-> $e $th1:rest) $th2:rest)
           :with $threads (!@ $x.$e !NL $th1 !NL $th2)))

(define-sexprw-tactic threadl
  $x:thread1able
  (-> $x.$threads))

';; example for thread tactic
(add1 (orelse (to-number '(1 2 3)) 0))

;; ----------------------------------------

(define-sexprw-nt threadNable-fun
  :attributes ($e $threads)
  (pattern ((!AND $f:id (!NOT ->>)) $e)
           :with $threads $f)
  (pattern ((!AND $f (!NOT ->>)) $args:rest1 $e)
           :with $threads ($f $args)))

(define-sexprw-nt threadNable
  :attributes ($threads)
  (pattern $x:threadNable-fun
           :with $threads (!@ $x.$e !NL $x.$threads))
  (pattern (->> $x:threadNable-fun $th:rest)
           :with $threads (!@ $x.$e !NL $x.$threads !NL $th))
  (pattern (->> (->> $e $th1:rest) $th2:rest)
           :with $threads (!@ $x.$e !NL $th1 !NL $th2)))

(define-sexprw-tactic threadr
  $x:threadNable
  (->> $x.$threads))

';; example for thread tactic
(sum (map add1 (filter values (map to-number '(1 2 3)))))

;; ============================================================
;; Conditionals

(define-sexprw-tactic clj-if-to-cond
  (if $test $then $else)
  (cond (!@ $test !SL $then) !NL
        (!@ :else !SL $else)))

' ;; example for clf-if-to-cond
(if a b c)

';; another example
(if (pos x)
    (do a thing)
  (forget about it))

(define-sexprw-tactic clj-condp/else
  (cond (!@ ($cmp $val:id $arg) $then) ... :else $else)
  :guard (lambda (env)
           (let ((cmp (sexprw-guard-all-same-atom env '$cmp))
                 (val (sexprw-guard-all-same-atom env '$val)))
             (and cmp val `((($cmp1 . ,cmp) ($val1 . ,val) . ,env)))))
  (condp $cmp1 $val1 !NL
         (!@ !NL $arg !SL $then) ... !NL
         $else))

';; example for condp
(cond (> x 90) 'a
      (> x 80) 'b
      (> x 70) 'c
      :else 'f)


;; ============================================================

(provide 'sexprw-for-clojure)
