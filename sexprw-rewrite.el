;;; sexprw-rewrite.el --- high-level patterns and tactics  -*- lexical-binding:t -*-

;; Copyright 2013-2019 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later;
;; see sexprw.el for details.

(require 'sexprw-core)
(require 'sexprw-desugar)

;; ============================================================

(defun sexprw-template (template env)
  "Produces (cons 'pre PreOutput) for given TEMPLATE and ENV."
  (cons 'pre (sexprw-template* (sexprw-desugar-pattern template t) env)))

;; ============================================================
;; Rewriting

(defvar sexprw-pattern-history nil)
(defvar sexprw-template-history nil)

(defun sexprw-rewrite (pattern template &optional guard)
  (interactive
   (list 
    (read-from-minibuffer "Pattern: " nil nil t 'sexprw-pattern-history)
    (read-from-minibuffer "Template: " nil nil t 'sexprw-template-history)))
  ;; (message "parsed pattern = %S" (sexprw-desugar-pattern pattern nil))
  (sexprw-rewrite/ast (sexprw-desugar-pattern pattern nil)
                      (sexprw-desugar-pattern template t)
                      guard))

(defun sexprw-rewrite/ast (pattern template &optional guard)
  (save-excursion
    (sexprw-skip-whitespace)
    (let* ((init-point (point))
           ;; puts point after pattern match
           (replacement (sexprw-compute-rewrite/ast pattern template guard)))
      (and replacement
           (progn
             (delete-and-extract-region init-point (point))
             (sexprw-emit replacement)
             t)))))

(defun sexprw-compute-rewrite/ast (pattern template &optional guard)
  ;; (message "pattern = %S" pattern)
  ;; (message "template = %S" template)
  (let ((env (sexprw-match pattern)))
    ;; (message "point = %S" (point))
    ;; (message "env = %S" env)
    (and env
         (sexprw-check-nonlinear-patterns (car env))
         (let ((env* (if guard (funcall guard (car env)) env)))
           ;; (message "guarded env = %S" env*)
           (and (or env*
                    (sexprw-fail `(guard env= ,env)))
                (let ((preoutput
                       (condition-case error-info
                           (sexprw-template* template (car env*))
                         (sexprw-template-error 
                          (sexprw-fail `(template ,error-info guard-env=
                                                  ,(car env*)))))))
                  ;; (message "preoutput = %S" preoutput)
                  (and preoutput
                       (let ((output
                              (condition-case error-info
                                  (sexprw-output preoutput)
                                (sexprw-template-error
                                 (sexprw-fail `(output ,error-info))))))
                         ;; (message "output = %S" output)
                         output))))))))

;; ============================================================
;; NT Definitions (see also same section in sexprw-core.el)

(defmacro define-sexprw-nt (name &rest clauses)
  ;; FIXME: Don't make such definitions global since different languages will
  ;; likely want different non-terminals.
  "Define NAME as a sexp-rewrite nonterminal specified by the CLAUSES."
  `(progn (put ',name 'sexprw-nt (sexprw-parse-nt-def ',clauses)) ',name))

(defun sexprw-parse-nt-def (clauses)
  (let ((docstring nil)
        (attrs nil))
    (when (and (consp clauses)
               (stringp (car clauses)))
      (setq docstring (car clauses))
      (setq clauses (cdr clauses)))
    (when (and (>= (length clauses) 2)
               (eq (car clauses) ':attributes))
      (setq attrs (cadr clauses))
      (dolist (attr attrs)
        (unless (symbolp attr)
          (error "Expected symbol for attribute: %S" attr)))
      (setq clauses (cddr clauses)))
    (let* ((patterns (mapcar #'sexprw-parse-clause clauses))
           (pattern (if (= 1 (length patterns))
                        (car patterns)
                      (cons 'OR patterns))))
      (list 'nt pattern attrs docstring))))

(defun sexprw-parse-clause (clause)
  (let ((parts clause)
        (pattern nil))
    (unless (and (consp parts)
                 (eq (car parts) 'pattern)
                 (>= (length parts) 2))
      (error "Bad sexp-rewrite nonterminal clause: %S" clause))
    (let ((pattern+parts (sexprw-parse-pattern+clauses (cdr parts) clause)))
      (setq pattern (car pattern+parts))
      (setq parts (cdr pattern+parts))
      (when parts
        (error "Bad clause options: %S" clause))
      pattern)))

(defun sexprw-parse-pattern+clauses (parts whole)
  ;; Returns (cons pattern leftover-parts)
  (let ((pattern nil))
    (unless (consp parts)
      (error "Missing pattern: %S" whole))
    (setq pattern (sexprw-desugar-pattern (car parts) nil))
    (setq parts (cdr parts))
    (while (and parts (keywordp (car parts)))
      (cond ((eq (car parts) ':guard)
             (unless (>= (length parts) 2)
               (error "Missing expression for :guard option: %S" whole))
             (setq pattern `(GUARD ,pattern ,(nth 1 parts)))
             (setq parts (nthcdr 2 parts)))
            ((eq (car parts) ':with)
             ;; FIXME: support (pvar ...), etc
             (unless (>= (length parts) 3)
               (error "Missing variable or template for :with option: %S" whole))
             (let* ((pvar (nth 1 parts))
                    (template (nth 2 parts))
                    (with-guard
                     `(lambda (env)
                        (let ((pre (sexprw-template ',template env)))
                          (list (cons (cons ',pvar pre) env))))))
               (setq pattern `(GUARD ,pattern ,with-guard))
               (setq parts (nthcdr 3 parts))))
            (t
             (error "Bad clause option keyword: %S" (car parts)))))
    (cons pattern parts)))

;; ============================================================
;; Built-in sexprw nonterminals

;; Sneaky tricks: 
;;  - (!SPLICE) is no-op pattern
;   - guard can use and move point (discouraged in user nts, though!)

(define-sexprw-nt pure-sexp
  :attributes ($)
  (pattern (!SPLICE)
           :guard (lambda (env)
                    (let ((next (sexprw-grab-next-sexp t)))
                      (and (or next
                               (sexprw-fail `(match var pure-sexp grab)))
                           (list (list (cons '$ next))))))))

(define-sexprw-nt sexp
  :attributes ($)
  (pattern (!SPLICE)
           :guard (lambda (env)
                    (let ((next (sexprw-grab-next-sexp nil)))
                      (and (or next
                               (sexprw-fail `(match var sexp grab)))
                           (list (list (cons '$ next))))))))

(define-sexprw-nt id
  :attributes ($)
  (pattern $x:pure-sexp
           :guard (lambda (env)
                    (let* ((x (sexprw-env-ref env '$x))
                           (pure-text (sexprw-block-pure-text x)))
                      ;; (message "x = %S" x)
                      ;; (message "pure-text = %S" pure-text)
                      (and (or (string-match sexprw-pure-atom-re pure-text)
                               (sexprw-fail `(match var sym atom)))
                           (list (list (cons '$ x))))))))

(define-sexprw-nt rest
  "Rest of matchable text"
  :attributes ($)
  (pattern (!SPLICE)
           :guard (lambda (env)
                    (sexprw-skip-whitespace) ;; FIXME: redundant?
                    (let ((init-point (point)))
                      (goto-char (point-max))
                      (let ((b (sexprw-range-to-block init-point nil (point))))
                        (list (list (cons '$ b))))))))

(define-sexprw-nt rest1
  "Rest but for one sexp"
  :attributes ($)
  (pattern (!SPLICE)
           :guard (lambda (env)
                    (sexprw-skip-whitespace) ;; FIXME: redundant?
                    (let ((init-point (point)))
                      (and (sexprw-skip-forward-to-n-sexps-before-end 1)
                           (let ((b (sexprw-range-to-block init-point nil (point))))
                             (list (list (cons '$ b)))))))))

;; ============================================================
;; Tactics

;; A Tactic is an nt that that defines $out and also has the property
;; 'sexprw-tactic (with value t).

(defun sexprw-tactic-symbolp (sym)
  (and (get sym 'sexprw-tactic) t))

(defmacro define-sexprw-tactic (name &rest parts)
  ;; FIXME: Don't make those rules global since different languages will
  ;; want different rules.
  "Define NAME as a sexprw-rewrite tactic."
  (unless (and name (symbolp name))
    (error "define-sexprw-tactic: expected symbol for NAME, got: %S" name))
  `(progn (put ',name 'sexprw-nt (sexprw-parse-tactic-defn ',name ',parts))
          (put ',name 'sexprw-tactic t)
          ',name))

(defun sexprw-parse-tactic-defn (name parts)
  (let* ((whole (cons 'define-sexprw-tactic (cons name parts)))
         (pattern+parts (sexprw-parse-pattern+clauses parts whole))
         (pattern (car pattern+parts))
         (template nil))
    (setq parts (cdr pattern+parts))
    (unless parts
      (error "Missing template: %S" whole))
    (setq template (car parts))
    (setq parts (cdr parts))
    (when parts
      (error "Extra terms after template: %S" whole))
    `(nt (GUARD ,pattern
                (lambda (env)
                  (let ((pre (sexprw-template ',template env)))
                    (list (cons (cons '$out pre) env)))))
         ($out) nil)))

(defvar sexprw-tactic-history nil)

(defun sexprw-read-tactic-from-minibuffer ()
  (intern
   (completing-read "Tactic: "
                    obarray
                    #'sexprw-tactic-symbolp
                    t
                    nil
                    'sexprw-tactic-history)))

;; ============================================================
;; Running Tactics

;; sexprw-run-tactic* functions return list of successful tactics in
;; reverse order

(defun sexprw-run-tactic (tactic-name)
  (let* ((nt-val (sexprw-nt-value tactic-name))
         (nt-pattern (nth 1 nt-val)))
    (and (let ((sexprw-current-operation `(tactic ,tactic-name))) ; fluid-let
           (sexprw-rewrite/ast nt-pattern '(VAR $out)))
         (list tactic-name))))

(defun sexprw-run-tactics-until-success (tactics &optional times0 disabled)
  (let ((times times0)
        success
        rused)
    (while (> times 0)
      (setq times (1- times))
      (setq success nil)
      (dolist (tactic tactics)
        (unless (memq tactic disabled)
          (unless success
            (when (sexprw-run-tactic tactic)
              (setq success t)
              (setq rused (cons tactic rused))))))
      (unless success (setq times 0)))
    rused))

;; ============================================================

(provide 'sexprw-rewrite)
;;; sexprw-rewrite.el ends here.
