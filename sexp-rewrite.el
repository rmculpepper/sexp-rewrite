;;; sexp-rewrite.el --- pattern-based rewriting of sexp-structured code

;;; Copyright 2013 Ryan Culpepper.
;;; Released under the terms of the GPL version 3 or later;
;;; see the text after `sexprw-legal-notice' for details.

(defconst sexprw-copyright    "Copyright 2013 Ryan Culpepper")
(defconst sexprw-version      "0.02")
(defconst sexprw-author-name  "Ryan Culpepper")
(defconst sexprw-author-email "ryanc@racket-lang.org")
(defconst sexprw-web-page     "https://github.com/rmculpepper/sexp-rewrite")

(defconst sexprw-legal-notice
  "This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License at http://www.gnu.org/licenses/gpl-3.0.html
for more details.")

;; ============================================================
;; TO DO

;; short term
;; - make sure sugared pattern lang is complete for core pattern lang
;; - automated testing
;; - documentation, rationale, etc
;; - documentation for individual tactics ??
;; - support COMMENT var kind
;; - better comment handling (custom regexp? may need hook)
;; - improve guard support 
;;   - require guard extends env?
;;   - add ranges back to block matches
;;     - might be useful for recursive processing ??
;; - package nicely in Emacs idiom
;;   - minor mode ??
;;   - make sure keybindings are added politely
;;   - customization options ??
;; - tweak whitespace handling ??
;; - hook for scan-sexps replacement
;; - hook for scan-whitespace, scan-comments replacements
;; - custom var to disable square brackets (use parens instead)
;; - build "tactic apropos"---search by literals in tactic pattern & template
;; - more interactive/flexible rewriting
;;   - eg, move let/let*/letrec bindings to <interactive point>

;; long term
;; - port to DrRacket
;; - use DrRacket semantic info (eg, freevars) for safety

;; ============================================================
;; Misc notes

;; Matching functions, etc return nil on failure, only raise error on
;; bad input (illegal pattern, etc).

;; ============================================================
;; Keybindings

(defvar sexprw-mode-keymap (make-sparse-keymap))
(global-set-key (kbd "C-c C-d") sexprw-mode-keymap)

(define-key sexprw-mode-keymap "e" 'sexprw-auto-expression)
(define-key sexprw-mode-keymap "d" 'sexprw-auto-definition)
(define-key sexprw-mode-keymap "x" 'sexprw-execute-tactic)
(define-key sexprw-mode-keymap "s" 'sexprw-search-pattern)
(define-key sexprw-mode-keymap "[" 'sexprw-squarify)
(define-key sexprw-mode-keymap "(" 'sexprw-roundify)

(define-key sexprw-mode-keymap "k" 'sexprw-kill-next-sexpragon-sexp)
(define-key sexprw-mode-keymap "w" 'sexprw-kill-sexpragon-region)
(define-key sexprw-mode-keymap "y" 'sexprw-yank-sexpragon)

(define-key sexprw-mode-keymap (kbd "M-SPC") 'sexprw-collapse-space/move-sexps)

(define-key sexprw-mode-keymap (kbd "r e")
  (lambda () (interactive) (sexprw-auto-expression 100)))
(define-key sexprw-mode-keymap (kbd "r d")
  (lambda () (interactive) (sexprw-auto-definition 100)))

(defvar sexprw-auto-expression-tactics nil
  "List of tactics tried by `sexprw-auto-expression'.")
(defvar sexprw-auto-definition-tactics nil
  "List of tactics tried by `sexprw-auto-definition'.")
  
(defvar sexprw-tactic-history nil)
(defvar sexprw-pattern-history nil)
(defvar sexprw-template-history nil)

(defgroup sexprw-group nil
  "Customization options for sexp-rewrite."
  :group 'scheme)

(defcustom sexprw-disabled-auto-tactics nil
  "Tactics that should not be run automatically.
Affects only `sexprw-auto-expression' and `sexprw-auto-definition';
disabled tactics can still be run via `sexprw-execute-tactic', etc."
  :type '(repeat symbol)
  :group 'sexprw-group)

(defun sexprw-disable-tactic (tactic-name)
  (interactive
   (list (sexprw-read-tactic-from-minibuffer)))
  (push tactic-name sexprw-disabled-auto-tactics))

(defun sexprw-enable-tactic (tactic-name)
  (interactive
   (list (sexprw-read-tactic-from-minibuffer)))
  (setq sexprw-disabled-auto-tactics
        (delete tactic-name sexprw-disabled-auto-tactics)))

(defun sexprw-auto-expression (&optional times)
  "Run the default sexp-rewrite tactics for expressions.
Customizable via the variable `sexprw-auto-expression-tactics'."
  (interactive "p")
  (sexprw-execute-tactics sexprw-auto-expression-tactics times t))
(defun sexprw-auto-definition (&optional times)
  "Run the default sexp-rewrite tactics for definitions.
Customizable via the variable `sexprw-auto-definition-tactics'."
  (interactive "p")
  (sexprw-execute-tactics sexprw-auto-definition-tactics times t))

(defun sexprw-execute-tactic (tactic-name &optional times0)
  "Read sexprw-rewrite tactic, then try to execute it."
  (interactive
   (list (sexprw-read-tactic-from-minibuffer)
         (prefix-numeric-value current-prefix-arg)))
  (sexprw-execute-tactics (list tactic-name) times0 nil))

(defun sexprw-execute-tactics (tactic-names times0 respect-disabled)
  (setq sexprw-failure-info nil)
  (let ((rused (sexprw-run-tactics-until-success tactic-names times0)))
    (cond ((consp rused)
           (cond ((= (length rused) 1)
                  (message "Applied tactic %s" (car rused)))
                 (t (message "Applied tactics: %s" (reverse rused)))))
          (t
           (cond ((= (length tactic-names) 1)
                  (message "Tactic %s not applicable" (car tactic-names)))
                 (t (message "No applicable tactic")))))))

;; sexprw-run-tactic* functions return list of successful tactics in
;; reverse order

(defun sexprw-run-tactic (tactic-name)
  (let* ((nt-val (sexprw-nt-value tactic-name))
         (nt-pattern (nth 1 nt-val)))
    (and (let ((sexprw-current-operation `(tactic ,tactic-name))) ; fluid-let
           (sexprw-rewrite/ast nt-pattern '(VAR $out)))
         (list tactic-name))))

(defun sexprw-run-tactics-until-success (tactics &optional times0 respect-disabled)
  (let ((times times0)
        success
        rused)
    (while (> times 0)
      (setq times (1- times))
      (setq success nil)
      (dolist (tactic tactics)
        (unless (memq tactic sexprw-disabled-auto-tactics)
          (unless success
            (when (sexprw-run-tactic tactic)
              (setq success t)
              (setq rused (cons tactic rused))))))
      (unless success (setq times 0)))
    rused))

;; ============================================================
;; Debugging and diagnostics

(defvar sexprw-current-operation nil
  "Name of currently executing operation.")

(defvar sexprw-failure-info nil
  "Information about last sexp-rewrite failure(s).")

(defun sexprw-fail (info)
  (push (cons sexprw-current-operation (cons (point) info)) sexprw-failure-info)
  nil)

(defun sexprw-show-failure-info ()
  (interactive)
  (message "%S" sexprw-failure-info))

(put 'sexprw-template-error
     'error-conditions
     '(error sexprw-template-error))
(put 'sexprw-template-error
     'error-message
     "Error instantiating template")

;; ============================================================
;; Rewriting

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
                         (template-error 
                          (sexprw-fail `(template ,error-info guard-env=
                                                  ,(car env*)))))))
                  ;; (message "preoutput = %S" preoutput)
                  (and preoutput
                       (let ((output
                              (condition-case error-info
                                  (sexprw-output preoutput)
                                (template-error
                                 (sexprw-fail `(output ,error-info))))))
                         ;; (message "output = %S" output)
                         output))))))))

;; FIXME: here's another quadratic function...
(defun sexprw-check-nonlinear-patterns (env0)
  (let ((ok t)
        (env env0))
    (while (and env ok)
      (let* ((entry1 (car env))
             (key1 (car entry1))
             (rest-env (cdr env)))
        (setq env rest-env)
        (let ((entry2 (assq key1 rest-env)))
          (when entry2
            (unless (sexprw-entry-equal (cdr entry1) (cdr  entry2))
              (sexprw-fail `(nonlinear-pvar ,key1 env= ,env0))
              (setq ok nil))))))
    ok))

(defun sexprw-entry-equal (a b)
  (cond ((and (eq (car a) 'rep) (eq (car b) 'rep)
              (= (length a) (length b)))
         (let ((as (cdr a)) 
               (bs (cdr b))
               (ok t))
           (while (and as bs)
             (setq ok (sexprw-entry-equal (car as) (car bs)))
             (setq as (cdr as))
             (setq bs (cdr bs)))
           ok))
        ((and (eq (car a) 'block) (eq (car b) 'block))
         ;; FIXME: could compare sexpragons (if exist), slightly more equalities
         (equal (sexprw-block-text a)
                (sexprw-block-text b)))
        (t nil)))

;; ============================================================
;; Pretty patterns and templates

;; PP ::= symbol         ~ (quote symbol)
;;      | $name:nt       ~ (VAR $name nt)      ; sigil is part of pvar name
;;      | $name          ~ (VAR $name sexp)
;;      | (PP*)          ~ (LIST P*)
;;      | (!@ PP*)       ~ (SPLICE P*)
;;      | (!SPLICE PP*)  ~ (SPLICE P*)
;;      | PP ...         ~ (pREP P <Pk>)       ; <Pk> is patterns that follow,
;;                                             ; grouped as splice
;;      | (!OR PP*)      ~ (OR P*)
;;      | (!AND PP*)     ~ (AND P*)
;;      | (!GUARD P expr)~ (GUARD P expr)

;; PT ::= like PP, with following additions and replacements:
;;      | (!SQ PT*)      ~ (SQLIST T*)
;;      | !NL            ~ (NL)
;;      | !SP            ~ (SP)
;;      | !SL            ~ (SL)
;;      | (!REP PT vars) ~ (tREP T vars)
;;      | PT ...         ~ (tREP T nil)        ; vars=nil means "auto"

(defun sexprw-desugar-pattern (pretty template)
  (cond ((null pretty)
         '(LIST))
        ((symbolp pretty)
         (sexprw-desugar-pattern-symbol pretty template))
        ((vectorp pretty)
         (if template
             (cons 'SQLIST (sexprw-desugar-pattern-list (append pretty nil) template))
           (cons 'LIST (sexprw-desugar-pattern-list (append pretty nil) template))))
        ((not (consp pretty))
         (error "Bad %s: %S" (if template "template" "pattern") pretty))
        ((memq (car pretty) '(!@ !SPLICE))
         (cons 'SPLICE (sexprw-desugar-pattern-list (cdr pretty) template)))
        ((eq (car pretty) '!SQ)
         (if template
             (cons 'SQLIST (sexprw-desugar-pattern-list (cdr pretty) template))
             (error "Bad pattern (!SQ not allowed): %S" pretty)))
        ((eq (car pretty) '!REP)
         (if template
             (list 'tREP (sexprw-desugar-pattern (nth 1 pretty)) (nth 2 pretty))
           (error "Bad pattern (!REP not allowed): %S" pretty)))
        ((eq (car pretty) '!OR)
         (if template
             (error "Bad template (!OR not allowed): %S" pretty)
           (cons 'OR
                 (mapcar (lambda (p) (sexprw-desugar-pattern p nil))
                         (cdr pretty)))))
        ((eq (car pretty) '!AND)
         (if template
             (error "Bad template (!AND not allowed): %S" pretty)
           (cons 'AND
                 (if (consp (cdr pretty))
                     (cons (sexprw-desugar-pattern (cadr pretty) nil)
                           (mapcar (lambda (p) (sexprw-desugar-pattern p nil))
                                   (cddr pretty)))
                   nil))))
        ((eq (car pretty) '!GUARD)
         (if template
             (error "Bad template (!GUARD not allowed): %S" pretty)
           (let* ((subpattern (sexprw-desugar-pattern (nth 1 pretty) nil))
                  (guard (nth 2 pretty)))
             (unless (functionp guard)
               (error "Bad template: guard is not a function: %S" pretty))
             (list 'GUARD subpattern guard))))
        (t ; list
         (cons 'LIST (sexprw-desugar-pattern-list pretty template)))))

(defun sexprw-desugar-pattern-symbol (pretty template)
  (let ((name (symbol-name pretty)))
    (cond ((and template (eq pretty '!NL))
           '(NL))
          ((and template (eq pretty '!SP))
           '(SP))
          ((and template (eq pretty '!SL))
           '(SL))
          ((eq pretty '...)
           (error "Misplaced ellipses: %S" pretty))
          ((string-match "^[!]" name)
           (error "Bad symbol in %s (reserved): %S"
                  (if template "template" "pattern")
                  pretty))
          ((string-match "^[$][_[:alpha:]][^:]*$" name)
           (if template
               `(VAR ,pretty)
             `(VAR ,pretty sexp)))
          ((string-match "^\\([$][_[:alpha:]][^:]*\\):\\([[:alpha:]].*\\)$" name)
           (let ((var (intern (match-string 1 name)))
                 (nt (intern (match-string 2 name))))
             (when nil ;; too early, prevents mutually recursive nts, forward refs, etc.
               (unless (sexprw-nt-symbolp nt)
                 (error "Bad pattern variable, no such sexpr-rewrite nonterminal: %S" pretty)))
             `(VAR ,var ,nt)))
          ((string-match "^[$]" name)
           (error "Bad pattern variable: %S" pretty))
          (t `(quote ,pretty)))))

(defun sexprw-desugar-pattern-list (pretty template)
  ;; Note: *not* same as (mapcar sexprw-desugar-pattern ....), 
  ;; because handles ellipses.
  (let ((rpretty (reverse pretty))
        (accum nil)
        (dots nil))
    (while rpretty
      (let ((p1 (car rpretty)))
        (setq rpretty (cdr rpretty))
        (cond ((eq p1 '...)
               (when dots (error "Repeated ellipses in pattern: %S" pretty))
               (setq dots t))
              (t
               (let ((pp1 (sexprw-desugar-pattern p1 template)))
                 (when dots
                   (setq dots nil)
                   (cond (template
                          (setq pp1 (list 'tREP pp1 nil)))
                         (t
                          (setq pp1 (list 'pREP pp1 (cons 'SPLICE accum)))
                          (setq accum nil))))
                 (push pp1 accum))))))
    (when dots (error "Misplaced dots at beginning of pattern: %S" pretty))
    accum))

;; ============================================================
;; Core patterns

;; P ::= (LIST P*)
;;     | (SPLICE P*)
;;     | (quote symbol)
;;     | (VAR symbol nt)
;;     | (pREP P Pk)
;;     | (AND P*)
;;     | (OR P*)
;;     | (GUARD P expr)
;;
;; Matching builds an alist mapping pvar symbols to EnvValue
;; EnvValue ::= Block
;;            | (list 'rep EnvValue)          ; representing depth>0 list
;;            | (list 'pre PreOutput)         ; representing computed output
;;
;; (pREP P Pk) means "P ... Pk": match as many P as possible s.t. still
;; possible to match Pk afterwards (then commit). Handling together
;; avoids (non-local) backtracking while supporting non-trivial Pks.

;; FIXME (or not): doesn't handle dotted-pair notation

;; TODO: support IMPURITY as kind, matches non-whitespace stuff
;; between (point) and next sexp.

(defconst sexprw-pure-atom-re
  ;; Note: vague approximation, doesn't distinguish numbers from symbols,
  ;; doesn't support \ and | escapes, etc, doesn't support Unicode chars.
  ;; FIXME: use [:alpha:] to capture more chars (Unicode) ???
  ;; FIXME: includes dot ?
  ;; FIXME: should be predicate, not regexp
  "^[-~!@$^&*_+=:./<>?a-zA-Z#0-9]+$")

(defun sexprw-match (pattern)
  "Matches the sexp starting at point against core PATTERN,
returning an \(list ENV) mapping the pattern variables of
PATTERN to fragments, or nil on failure.  Advances point to end
of matched term(s)."
  ;; (message "matching (%S): %S" (point) pattern)
  (cond ((not (consp pattern))
         (error "Bad pattern: %s" pattern))
        ((eq (car pattern) 'quote)
         ;; Note: grabs pure-sexp, checks contains symbol
         (let ((next (sexprw-grab-next-sexp t)))
           (and (or next
                    (sexprw-fail `(match quote pure-sexp)))
                (let ((pure-text (sexprw-block-pure-text next)))
                  (and (or (string-match sexprw-pure-atom-re pure-text)
                           (sexprw-fail `(match quote is-symbol)))
                       (or (equal pure-text (symbol-name (cadr pattern)))
                           (sexprw-fail
                            `(match quote equal
                                    ,(symbol-name (cadr pattern)))))
                       (list nil))))))
        ((eq (car pattern) 'VAR)
         (sexprw-match-var (nth 1 pattern) (nth 2 pattern)))
        ((eq (car pattern) 'LIST)
         (sexprw-match-list (cdr pattern)))
        ((eq (car pattern) 'SPLICE)
         (sexprw-match-patterns (cdr pattern)))
        ((eq (car pattern) 'pREP)
         (sexprw-match-rep (nth 1 pattern) (nth 2 pattern)))
        ((eq (car pattern) 'OR)
         (let ((init-point (point))
               (result nil)
               (rfailinfos nil)
               (alternatives (cdr pattern)))
           (while (and (consp alternatives) (not result))
             (goto-char init-point)
             (let ((sexprw-failure-info nil)) ;; fluid-let
               (setq result (sexprw-match (car alternatives)))
               (push sexprw-failure-info rfailinfos))
             (setq alternatives (cdr alternatives)))
           (or result
               (sexprw-fail `(match or inners= ,(reverse rfailinfos))))))
        ((eq (car pattern) 'AND)
         (let ((init-point (point))
               (renvs nil)
               (ok t)
               (first-time t)
               (conjuncts (cdr pattern)))
           ;; Use restriction and looking-at (below) to ensure that
           ;; all conjuncts match the same sexps.
           ;; In other words, first conjunct constrains what
           ;; subsequent conjuncts can see.
           (save-restriction
             (while (and ok (consp conjuncts))
               (goto-char init-point)
               (let ((result (sexprw-match (car conjuncts))))
                 (cond ((and result
                             (or first-time
                                 (looking-at
                                  (concat sexprw-all-whitespace-re "\\'"))))
                        (setq first-time nil)
                        (push (car result) renvs)
                        (narrow-to-region init-point (point)))
                       (t
                        (setq ok nil))))
               (setq conjuncts (cdr conjuncts)))
             (and ok (list (apply #'append (reverse renvs)))))))
        ((eq (car pattern) 'GUARD)
         (let ((result (sexprw-match (nth 1 pattern)))
               (guard (nth 2 pattern)))
           (and result
                (let ((env (car result)))
                  (or (sexprw-check-guard-result (funcall guard env) env)
                      (sexprw-fail `(match guard env= ,env)))))))
        (t (error "Bad pattern: %S" pattern))))

;; TODO: check result is nil or (list extension-of-env)?
(defun sexprw-check-guard-result (result env)
  result)

(defun sexprw-match-var (pvar nt)
  (unless (sexprw-nt-symbolp nt)
    (error "Not defined as sexp-rewrite nt: %S" nt))
  (sexprw-skip-whitespace)
  (let* ((init-point (point))
         (nt-val (sexprw-nt-value nt))
         (nt-pattern (nth 1 nt-val))
         (nt-attrs (nth 2 nt-val)))
    (let ((result (sexprw-match nt-pattern)))
      (and result
           (sexprw-check-nonlinear-patterns (car result))
           (let ((env (sexprw-adj-env (car result) nt nt-attrs pvar)))
             (unless (assq pvar env)
               (let ((b (sexprw-range-to-block init-point nil (point))))
                 (push (cons pvar b) env)))
             (if (eq pvar '$_)
                 (list nil)
               (list env)))))))

(defun sexprw-adj-env (env nt attrs prefix)
  "Checks, restricts, and prefixes ENV."
  (let ((new-env nil))
    (dolist (attr attrs)
      (let ((entry (assq attr env)))
        (unless entry
          (error "Nonterminal `%S' did not bind attribute `%S'" nt attr))
        (let ((prefixed-attr
               (if (eq attr '$)
                   prefix
                 (intern (format "%s.%s" prefix attr)))))
          (push (cons prefixed-attr (cdr entry)) new-env))))
    (reverse new-env)))

;; returns t on success, nil if fewer than n sexps before end
(defun sexprw-skip-forward-to-n-sexps-before-end (n)
  (cond ((zerop n)
         (goto-char (point-max)))
        (t (let ((fast (point))
                 (slow (point)))
             (setq fast (ignore-errors (scan-sexps fast n)))
             (and fast
                  (progn
                    (while fast
                      (setq fast (ignore-errors (scan-sexps fast 1)))
                      (when fast (setq slow (scan-sexps slow 1))))
                    (goto-char slow)
                    t))))))

(defun sexprw-match-list (inners)
  (let ((next (sexprw-grab-next-sexp t)))
    (and (or next
             (sexprw-fail `(match-list grab)))
         (member (substring (sexprw-block-pure-text next) 0 1) '("(" "[" "{"))
         ;; narrow to just after start, just before end
         (let ((result
                (save-excursion
                  (save-restriction
                    (let ((start (sexprw-block-pure-start-position next))
                          (end (sexprw-block-end-position next)))
                      (goto-char (1+ start))
                      (narrow-to-region (1+ start) (1- end))
                      (let ((result (sexprw-match-patterns inners)))
                        (and result
                             (or (looking-at (concat sexprw-all-whitespace-re "\\'"))
                                 (sexprw-fail `(match-list end check-whitespace)))
                             result)))))))
           ;; save-excursion resets point to end of list
           result))))

(defun sexprw-match-patterns (inners)
  (let ((accum (list '()))) ; nil or (list alist)
    (dolist (inner inners)
      (when accum
        (let ((inner-result (sexprw-match inner)))
          (setq accum (and inner-result
                           (list (append (car inner-result) (car accum))))))))
    accum))

(defun sexprw-match-rep (inner after)
  ;; FIXME: add failure info
  (let ((matches nil))
    ;; matches : (listof (list match-count reversed-env-list point))
    ;; Each entry is after successfully matching inner match-count times.
    ;; Stage 1: build up matches of inner pattern
    (let ((count 0)
          (renvs nil)
          (last-point (point))
          (proceed t))
      (push (list count renvs last-point) matches)
      (while proceed
        (let ((next-result (sexprw-match inner)))
          (cond ((and next-result (> (point) last-point))
                 (setq count (1+ count))
                 (setq last-point (point))
                 (push (car next-result) renvs)
                 (push (list count renvs last-point) matches))
                (t
                 (setq proceed nil))))))
    ;; Stage 2: search for match that satisfies after pattern
    (let ((answer nil))
      (while (and matches (not answer))
        (let* ((match0 (car matches))
               (match-renvs (nth 1 match0))
               (match-point (nth 2 match0)))
          (setq matches (cdr matches))
          (goto-char match-point)
          (let ((next-result (sexprw-match after)))
            (when next-result
              (let* ((env (sexprw-reverse-merge-alists inner match-renvs))
                     (env (append (car next-result) env)))
                (setq answer (list env)))))))
      answer)))

;; FIXME: quadratic
(defun sexprw-reverse-merge-alists (inner alists)
  ;; Not every key might appear in every alist, due to OR patterns.
  (let ((keys (delete-dups (sexprw-pattern-variables inner nil)))
        (accum nil))
    (dolist (key keys)
      (let ((values nil))
        (dolist (alist alists)
          (let ((kv (assq key alist)))
            (when kv (push (cdr kv) values))))
        ;; Don't reverse values; thus "reverse merge" alists
        (push (cons key (cons 'rep values)) accum)))
    accum))

(defun sexprw-pattern-variables (pattern onto)
  ;; Accept templates too
  (cond ((eq (car pattern) 'VAR)
         (when (> (length pattern) 2)
           (let* ((pvar (nth 1 pattern))
                  (nt (nth 2 pattern))
                  (nt-val (sexprw-nt-value nt)))
             (let ((attrs (nth 2 nt-val)))
               (dolist (attr attrs)
                 (unless (eq attr '$)
                   (push (intern (format "%s.%s" pvar attr)) onto))))))
         (cons (nth 1 pattern) onto))
        ((memq (car pattern) '(LIST SPLICE SQLIST OR))
         (dolist (inner (cdr pattern))
           (setq onto (sexprw-pattern-variables inner onto)))
         onto)
        ((eq (car pattern) 'pREP)
         (sexprw-pattern-variables (nth 1 pattern) 
                                   (sexprw-pattern-variables (nth 2 pattern) onto)))
        ((eq (car pattern) 'tREP)
         (sexprw-pattern-variables (nth 1 pattern) onto))
        ((memq (car pattern) '(quote SP NL SL))
         onto)
        (t (error "Bad pattern: %S" pattern))))

;; ----

;; A Block is (list 'block TEXT ONELINEP STARTCOL IMPUREPREFIX START END).

(defun sexprw-block-text (block)
  (nth 1 block))
(defun sexprw-block-onelinep (block)
  (nth 2 block))
(defun sexprw-block-start-column (block)
  (nth 3 block))
(defun sexprw-block-impure-prefix (block)
  (nth 4 block))
(defun sexprw-block-start-position (block)
  (nth 5 block))
(defun sexprw-block-end-position (block)
  (nth 6 block))

(defun sexprw-block-purep (block)
  (zerop (sexprw-block-impure-prefix block)))

(defun sexprw-block-pure-start-position (block)
  (let ((start (sexprw-block-start-position block))
        (impure-prefix (sexprw-block-impure-prefix block)))
    (unless impure-prefix
      (error "Block has unknown contents"))
    (+ start impure-prefix)))

(defun sexprw-block-pure-text (block)
  (let ((text (sexprw-block-text block))
        (impure-prefix (sexprw-block-impure-prefix block)))
    (cond ((null impure-prefix)
           (error "Block has unknown contents"))
          ((zerop impure-prefix)
           text)
          (t (substring text 0 impure-prefix)))))

(defun sexprw-block-sexpragon (block)
  (let* ((text (sexprw-block-text block))
         (start-col (sexprw-block-start-column block)))
    (sexprw-sexpragon text start-col)))

(defun sexprw-grab-next-sexp (require-pure)
  "Grabs next sexp and returns Block or nil.

A Block is (list 'block TEXT ONELINEP STARTCOL IMPUREPREFIX START END).
TEXT is a string containing the contents of the block. ONELINEP
indicates if the block consists of a single line.

If IMPUREPREFIX is an integer, the block represents a single sexp
preceeded by comments, and IMPUREPREFIX is the number of
characters before the start of the sexp. If IMPUREPREFIX is nil,
then TEXT may represent multiple sexps or something else
entirely.

If REQUIRE-PURE is non-nil, then there must be no non-whitespace
characters before the start of the sexp, or else nil is returned.

On success, advances point to end of sexp."
  (let ((result (sexprw-grab-next-sexp-range)))
    (and result
         (let ((nonws-point (nth 1 result))
               (start-point (nth 2 result))
               (end-point (nth 3 result)))
           (and (or (not require-pure)
                    (= nonws-point start-point))
                (progn
                  (goto-char end-point)
                  (sexprw-range-to-block nonws-point
                                         start-point
                                         end-point)))))))

(defun sexprw-range-to-block (start pure-start end)
  (list 'block
        (filter-buffer-substring start end)
        (= (line-number-at-pos start)
           (line-number-at-pos end))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char start)
            (- (point) (line-beginning-position))))
        (and pure-start (- pure-start start))
        start
        end))

(defun sexprw-grab-next-sexp-range ()
  ;; FIXME/BUG: backwards scan loses things like quote prefix, 
  ;; can lead to treating "'x" as atomic sexp (shouldn't be).
  ;; Maybe add custom comment handling to avoid backwards scan?
  "Returns (list INIT-POINT NONWS-POINT START-POINT END-POINT) or nil.
INIT-POINT is where point started. NONWS-POINT is the location of
the first non-whitespace character. START-POINT is where the sexp
starts.  END-POINT is where the sexp ends.  Does not change
point."
  (condition-case error-info
      (save-excursion
        (let ((init-point (point)))
          (sexprw-skip-whitespace)
          (let* ((nonws-point (point))
                 (end-point (scan-sexps nonws-point 1))
                 (start-point (and end-point (scan-sexps end-point -1))))
            ;; scan-sexps signals error if EOF inside parens,
            ;; returns nil if EOF no sexp found
            (cond ((and start-point
                        (< start-point end-point))
                   (list init-point nonws-point start-point end-point))
                  (t nil)))))
    (scan-error
     ;; (message "Error is %s" error-info)
     nil)))

(defun sexprw-skip-whitespace ()
  (skip-chars-forward "[:space:]\n"))

;; ============================================================
;; Guard utilities

(defun sexprw-env-ref (env key)
  "Fetch the value associated with KEY in ENV, or nil otherwise."
  (let ((result (assq key env)))
    (and result (cdr result))))

(defun sexprw-guard-all-distinct (env &rest pvars)
  "Check that all of the atoms bound to the PVARS are distinct.
If there is a duplicate, or if any PVAR has a non-atom binding, return nil.
On success, return (list ENV), so suitable as the body of a guard function."
  (let ((seen (make-hash-table :test 'equal))
        (worklist nil)
        (failed nil))
    (dolist (pvar pvars)
      (setq worklist (list (sexprw-env-ref env pvar)))
      (while (and worklist (not failed))
        (let ((item (car worklist)))
          (setq worklist (cdr worklist))
          (cond ((eq (car item) 'atom)
                 (when (gethash (cadr item) seen nil)
                   (setq failed t))
                 (puthash (cadr item) seen t))
                ((eq (car item) 'rep)
                 (setq worklist (append (cdr item) worklist)))
                (t
                 (error "Non-atom value for pvar '%s': %S" pvar item)
                 (setq failed t))))))
    (and (or (not failed)
             (sexprw-fail `(guard all-distinct ,pvars)))
         (list env))))

(defun sexprw-guard-no-dot (env &rest pvars)
  "Check that none of the atoms bound to the PVARS is a dot.
On failure, return nil; on success, return (list ENV), so suitable as
guard body."
  (let ((worklist nil)
        (failed nil))
    (dolist (pvar pvars)
      (setq worklist (list (sexprw-env-ref env pvar)))
      (while (and worklist (not failed))
        (let ((item (car worklist)))
          (setq worklist (cdr worklist))
          (cond ((eq (car item) 'block)
                 (when (equal (sexprw-block-pure-text item) ".")
                   (setq failed t)))
                ((eq (car item) 'rep)
                 (setq worklist (append (cdr item) worklist)))
                (t
                 (error "Bad value for pvar '%s': %S" pvar item))))))
    (and (or (not failed)
             (sexprw-fail `(guard no-dot)))
         (list env))))


;; ============================================================
;; Templates
;;
;; T ::= string          ; literal text, eg "\n" inserts non-latent newline
;;     | (quote symbol)  ; literal symbol
;;     | (VAR symbol)    ; pattern variable
;;     | (LIST T*)       ; parenthesized list
;;     | (SQLIST T*)     ; bracketed list
;;     | (SPLICE T*)     ; spliced list contents
;;     | (SP)            ; latent space (ie, change latent newline to latent
;;     |                 ; space)

;;     | (SL)            ; latent "soft" newline: if surrounding list has any 
;;                       ; NLs or multi-line blocks, NL, else ignore
;;     | (NL)            ; latent newline
;;     | (tREP T vars)   ; repetition
;;
;; PreOutput = (treeof PreOutputPart)
;; PreOutputPart =
;;   - string
;;   - 'SP
;;   - 'NL
;;   - 'SL
;;   - 'NONE
;;   - (cons 'SEXPRAGON listofstring)
;;   - (cons 'SL=nil PreOutput)
;;   - (cons 'SL=NL PreOutput)
;; Interpret PreOutput left to right; *last* spacing symbol to occur wins.
;;
;; Output = (listof (U string 'NL (cons 'SEXPRAGON listofstring)))

(defun sexprw-template (template env)
  "Produces (cons 'pre PreOutput) for given TEMPLATE and ENV."
  (cons 'pre (sexprw-template* (sexprw-desugar-pattern template t) env)))

;; sexprw-template*-multiline : boolean, fluid
;; Set when (hard) NL or multi-line block occurs in current LIST/SQLIST.
(defvar sexprw-template*-multiline nil) ;; fluid

(defun sexprw-template* (template env)
  "Interprets core TEMPLATE using the pattern variables of ENV."
  ;; (message "** template = %S" template)
  (cond ((stringp template)
         template)
        ((not (consp template))
         (error "Bad template: %S" template))
        ((eq (car template) 'quote)
         (list (symbol-name (cadr template))
               'SP))
        ((eq (car template) 'VAR)
         (sexprw-template-var (cadr template) env))
        ((memq (car template) '(LIST SQLIST))
         (let ((open (if (eq (car template) 'LIST) "(" "["))
               (close (if (eq (car template) 'LIST) ")" "]"))
               (multiline nil))
           (let ((contents
                  (let ((sexprw-template*-multiline nil)) ;; fluid-let
                    (prog1 (sexprw-template-list-contents (cdr template) env)
                      (setq multiline sexprw-template*-multiline)))))
             (when multiline (setq sexprw-template*-multiline t))
             (list open
                   (cons (if multiline 'SL=NL 'SL=nil) contents)
                   'NONE
                   close
                   'SP))))
        ((eq (car template) 'SPLICE)
         (sexprw-template-list-contents (cdr template) env))
        ((memq (car template) '(SP NL SL))
         (car template))
        ((eq (car template) 'tREP)
         (sexprw-template-rep template env))))

(defun sexprw-template-rep (template env)
  ;; (message "env for rep = %S" env)
  (let* ((inner (nth 1 template))
         (vars (or (nth 2 template)
                   ;; Take *all* depth>0 pvars in env that occur in template
                   ;; (beware duplicate keys in alist)
                   (let* ((env-keys (sexprw-pattern-variables template '()))
                          ;; FIXME: Ack! quadratic, mutates, etc
                          (env-keys (delete-dups env-keys))
                          (raccum '()))
                     (dolist (key env-keys)
                       (when (eq (car (cdr (assq key env))) 'rep)
                         (setq raccum (cons key raccum))))
                     (reverse raccum))))
         (vals (mapcar (lambda (pvar)
                         (let ((entry (assq pvar env)))
                           (unless entry
                             (error "No entry for pvar '%s' in: %S" pvar env))
                           (let ((value (cdr entry)))
                             (unless (and (consp value) (eq (car value) 'rep))
                               (error "Value for pvar '%s' is not list (depth error): %S"
                                      pvar entry))
                             (cdr value))))
                       vars)))
    (unless vars (error "No repetition vars for tREP: %S" template))
    (let* ((lengths (mapcar #'length vals))
           (length1 (car lengths)))
      (dolist (l lengths)
        (unless (= l length1)
          (signal 'template-error 'ellipsis-count-mismatch)))
      (let ((raccum '()))
        (dotimes (_i length1)
          (let* ((extenv+vals (sexprw-split/extend-env vars vals env))
                 (extenv (car extenv+vals)))
            (setq vals (cdr extenv+vals))
            (setq raccum 
                  (cons (sexprw-template* inner extenv)
                        raccum))))
        (reverse raccum)))))

(defun sexprw-split/extend-env (vars vals env)
  (let* ((val1s (mapcar #'car vals))
         (rests (mapcar #'cdr vals)))
    (while vars
      (setq env (cons (cons (car vars) (car val1s)) env))
      (setq vars (cdr vars))
      (setq val1s (cdr val1s)))
    (cons env rests)))

(defun sexprw-template-var (pvar env)
  (let ((entry (assq pvar env)))
    (unless entry
      (error "No entry for pvar '%s'" pvar))
    (let ((value (cdr entry)))
      (cond ((and (consp value) (eq (car value) 'block))
             (let ((text (sexprw-block-text value))
                   (lines (sexprw-block-sexpragon value))
                   (space (if (sexprw-block-onelinep value) 'SP 'NL)))
               (unless (sexprw-block-onelinep value)
                 (setq sexprw-template*-multiline t))
               (cond ((zerop (length text))
                      ;; no space after empty block
                      null)
                     (lines
                      (list (cons 'SEXPRAGON lines) space))
                     (t
                      (list text space)))))
            ((and (consp value) (eq (car value) 'pre))
             ;; 'pre entry should already include trailing space
             (cdr value))
            ((and (consp value) (eq (car value) 'rep))
             (error "Depth error for pvar '%s'; value is: %S" pvar value))
            (t (error "Bad pvar value for pvar '%s': %s" pvar value))))))

(defun sexprw-template-list-contents (inners env)
  ;; We don't add inter-element spacing here; 
  ;; each element should add its own trailing spacing.
  (let ((accum (list '()))) ; nil or (list PreOutput)
    (dolist (inner inners)
      (setq accum (cons accum (sexprw-template* inner env))))
    accum))

(defun sexprw-output (pre)
  (let* ((result (sexprw-output* pre nil 'NONE))
         (raccum (car result))
         (latent (cdr result)))
    (let ((sexprw-output*-SL nil)) ;; fluid-let
      (reverse raccum))))

;; sexprw-output*-SL : (U nil 'NL), fluid
(defvar sexprw-output*-SL nil)

(defun sexprw-output* (pre raccum latent)
  (cond ((and (consp pre) (eq (car pre) 'SEXPRAGON))
         (let* ((raccum (cons (sexprw-output*-spacing latent) raccum))
                (raccum (cons pre raccum)))
           (cons raccum 'NONE)))
        ((and (consp pre) (eq (car pre) 'SL=nil))
         (let ((sexprw-output*-SL nil)) ;; fluid-let
           (sexprw-output* (cdr pre) raccum latent)))
        ((and (consp pre) (eq (car pre) 'SL=NL))
         (let ((sexprw-output*-SL 'NL)) ;; fluid-let
           (sexprw-output* (cdr pre) raccum latent)))
        ((consp pre)
         (let ((result (sexprw-output* (car pre) raccum latent)))
           (sexprw-output* (cdr pre) (car result) (cdr result))))
        ((stringp pre)
         (let* ((raccum (cons (sexprw-output*-spacing latent) raccum))
                (raccum (cons pre raccum)))
           (cons raccum 'NONE)))
        ((null pre)
         (cons raccum latent))
        ((symbolp pre)
         (cons raccum
               (if (eq pre 'SL) (or sexprw-output*-SL latent) pre)))
        (t
         (error "Bad pre-output: %S" pre))))

(defun sexprw-output*-spacing (spacing)
  (cond ((eq spacing 'NL) 'NL)
        ((eq spacing 'SP) " ")
        ((eq spacing 'NONE) "")
        (t (error "Bad spacing: %S" spacing))))

(defun sexprw-emit (output)
  (while output
    (let ((fragment (car output)))
      (setq output (cdr output))
      (cond ((eq fragment 'NL)
             (newline-and-indent))
            ((stringp fragment)
             (insert fragment))
            ((and (consp fragment) (eq (car fragment) 'SEXPRAGON))
             (sexprw-emit-sexpragon (cdr fragment)))
            (t (error "Bad output: %S" (car output)))))))

;; ============================================================
;; Convert to square brackets

(defun sexprw-squarify (&optional times)
  "Turn round parens into square brackets."
  (interactive "P")
  (let ((times (cond ((numberp times) times)
                     ((consp times) (car times))
                     ((null times) nil))))
    (save-excursion
      (sexprw-rebracket-repeat times "(" "[" "]" "parenthesis"))
    nil))

(defun sexprw-roundify (&optional times)
  "Turn square brackets into round parens."
  (interactive "P")
  (let ((times (cond ((numberp times) times)
                     ((consp times) (car times))
                     ((null times) nil))))
    (save-excursion
      (sexprw-rebracket-repeat times "[" "(" ")" "square bracket"))
    nil))

(defconst sexprw-all-whitespace-re 
  ;; "\\(\\s-\\|[\n]\\)*"  ; ??? matches close parens too?
  "[[:space:]\n]*")

(defun sexprw-open-bracket-re (from)
  ;; (concat "[[:space:]]*" (regexp-quote from))
  ;; (concat "\\s-*" (regexp-quote from))  ; doesn't get newlines
  (concat sexprw-all-whitespace-re (regexp-quote from)))

(defun sexprw-rebracket-once (from to-open to-close bracket-name)
  (cond ((looking-at (sexprw-open-bracket-re from))
         (let (end)
           (forward-list 1)
           (setq end (1- (point)))
           (backward-list 1)
           (delete-char 1)
           (insert to-open)
           (goto-char end)
           (delete-char 1)
           (insert to-close)
           ;; (goto-char (1+ (point)))
           ))
        (t
         (message "Not at open %s" bracket-name))))

(defun sexprw-rebracket-repeat (times from to-open to-close bracket-name)
  (let ((start-re (sexprw-open-bracket-re from)))
    (while (and (looking-at start-re)
                (or (not times) (> times 0)))
      (when times (setq times (1- times)))
      (sexprw-rebracket-once from to-open to-close bracket-name))))

;; ============================================================
;; Search with patterns

(defun sexprw-search-pattern (pattern)
  "Search forward for sexp matching PATTERN."
  (interactive
   (list (read-from-minibuffer "Search pattern: " nil nil t
                               'sexprw-pattern-history)))
  (let ((sexrpw-current-operation 'search)) ;; fluid-let
    (setq sexprw-failure-info nil)
    (sexprw-search-pattern/ast (sexprw-desugar-pattern pattern nil))))

(defun sexprw-search-pattern/ast (pattern)
  ;; (message "search pattern = %S" pattern)
  (let ((init-point (point))
        (success nil)
        (continue t))
    (while continue
      (setq continue nil)
      (sexprw-skip-whitespace)
      (let ((result (save-excursion (sexprw-match pattern))))
        (cond (result
               (setq success t))
              (t
               (setq continue (sexprw-move-forward))))))
    (cond (success
           (push-mark init-point)
           (message "Pattern found; mark saved where search started"))
          (t
           (goto-char init-point)
           (message "Pattern not found")))))

(defun sexprw-move-forward ()
  "Moves point forward along sexp boundaries.
Can move forward by skipping whitespace, moving to start of next
sexp, moving to end of next sexp, moving into list, or moving out
of list."
  (let* ((init-point (point))
         (next-sexp-end (ignore-errors (scan-sexps init-point 1)))
         (next-sexp-start (and next-sexp-end
                               (ignore-errors (scan-sexps next-sexp-end -1))))
         (next-list-start (ignore-errors (scan-lists init-point 1 -1))))
    ;; (message "next-sexp-end = %s, next-list-start = %s"
    ;;          next-sexp-end next-list-start)
    (cond ((and next-sexp-start (> next-sexp-start init-point))
           ;; (message "Going to start of next sexp")
           (goto-char next-sexp-start)
           t)
          ((not next-sexp-end)
           ;; try going up
           ;; (message "Going up")
           (progn (ignore-errors (up-list 1)) (> (point) init-point)))
          ((or (not next-list-start)
               (> next-list-start next-sexp-end))
           ;; (message "Going forward")
           ;; next sexp is not a list
           (goto-char next-sexp-end)
           t)
          (t
           ;; (message "Going down")
           (progn (ignore-errors (down-list 1)) (> (point) init-point))))))

;; ============================================================
;; Sexpragon functions

;; A "sexpragon" is the shape of a well-formatted sexp:

;;     (----------+
;;     |          |
;;     |      +---+
;;     +------)

;; There must be no non-whitespace characters to the left of the open
;; paren's column from the second line to the last. Well-formatted
;; Lisp/Scheme/Racket code is nearly always sexpragonal, with the
;; occasional exception of multi-line string literals.

(defun sexprw-sexpragon (string start-col)
  (let* ((lines (split-string text "[\n]" nil))
         (ok t)
         (rtext nil))
    ;; First line already has indentation removed
    (push (car lines) rtext)
    (setq lines (cdr lines))
    ;; Process successive lines
    (while (and ok lines)
      (let* ((line (car lines))
             (line-end (length line))
             (col (min start-col line-end)))
        (if (string-match "^ *$" (substring line 0 col))
            (push (substring line col) rtext)
          (setq ok nil)))
      (setq lines (cdr lines)))
    (and ok (reverse rtext))))

(defun sexprw-kill-next-sexpragon-sexp ()
  "Kills the sexp at point, preserving relative indentation.
The sexp must be a sexpragon. Whitespace is removed from lines
after the first so the sexp will be properly indented when
`yank'ed at column 0 or yanked via `sexprw-yank-sexpragon'."
  (interactive)
  (let* ((init-point (point))
         (next (sexprw-grab-next-sexp-range)))
    (unless next
      (error "No sexp at point"))
    (let* ((start (nth 1 next))
           (end (nth 3 next))
           (lines (sexprw-sexpragon (filter-buffer-substring start end))))
      (unless lines
        (error "Non-sexpragonal sexp at point"))
      (let ((text (mapconcat 'identity lines "\n")))
        (delete-and-extract-region init-point end)
        (kill-new text)))))

(defun sexprw-kill-sexpragon-region (start end)
  "Kills from START to END, preserving relative indentation.
The region must be a sexpragon. Whitespace is removed from lines
after the first so the sexp will be properly indented when
`yank'ed at column 0 or yanked via `sexprw-yank-sexpragon'."
  (interactive "r")
  (let ((text (filter-buffer-substring start end))
        (start-col (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char start)
                       (- start (line-beginning-position))))))
    (let ((lines (sexprw-sexpragon text start-col)))
      (unless lines
        (error "Non-sexpragonal region"))
      (let ((text (mapconcat 'identity lines "\n")))
        (delete-and-extract-region start end)
        (kill-new text)))))

(defun sexprw-yank-sexpragon ()
  "Yanks text, preserving relative indentation of multi-line text.
Whitespace is added to lines after the first so each line starts
at the same column as the first line."
  (interactive)
  (let ((text (current-kill 0))
        (col (- (point) (line-beginning-position))))
    (unless text
      (error "No text in kill ring"))
    (sexprw-emit-sexpragon (split-string text "[\n]" nil))))

(defun sexprw-emit-sexpragon (lines)
  (let ((col (save-restriction
               (widen)
               (- (point) (line-beginning-position)))))
    (when lines
      (insert (car lines))
      (setq lines (cdr lines)))
    (while lines
      (insert "\n")
      (unless (zerop (length (car lines)))
        (indent-to col))
      (insert (car lines))
      (setq lines (cdr lines)))))

;; ============================================================

;; sexp-rewrite nonterminal names have property 'sexprw-nt
;; with value (list 'nt P attrs docstring), where attrs is list of symbol

(defmacro define-sexprw-nt (name &rest clauses)
  "Define NAME as a sexp-rewrite nonterminal specified by the CLAUSES."
  `(progn (put ',name 'sexprw-nt (sexprw-parse-nt-def 'name ',clauses)) ',name))

(defun sexprw-parse-nt-def (name clauses)
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

(defun sexprw-nt-symbolp (sym)
  (and (get sym 'sexprw-nt) t))

(defun sexprw-nt-value (sym)
  (or (and (symbolp sym) (get sym 'sexprw-nt))
      (error "Not a sexp-rewrite nt name: %S" sym)))

;; ============================================================

;; A sexp-rewrite tactic name is an nt that that defines $out and also
;; has the property 'sexprw-tactic.

(defmacro define-sexprw-tactic (name &rest parts)
  "Define NAME as a sexprw-rewrite tactic."
  (unless (and name (symbolp name))
    (error "define-sexprw-tactic: expected symbol for NAME, got: %S" name))
  `(progn (put ',name 'sexprw-nt (sexprw-parse-tactic-defn ',name ',parts))
          (put ',name 'sexprw-tactic t)
          ',name))

(defun sexprw-tactic-symbolp (sym)
  (and (get sym 'sexprw-tactic) t))

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

(defun sexprw-read-tactic-from-minibuffer (&optional prompt history)
  (intern
   (completing-read (or prompt "Tactic: ")
                    obarray
                    'sexprw-tactic-symbolp
                    t
                    nil
                    (or history 'sexprw-tactic-symbolp))))

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

(defun sexprw-collapse-space/move-sexps (count)
  "Collapse space after point, moving COUNT (or all) following sexps.
If COUNT is nil, moves all following sexps."
  (interactive "P") 
  (when (consp count) (setq count (car count)))
  (unless (integerp count) (setq count nil))
  (save-excursion
    (let ((init-point (point)))
      (sexprw-skip-whitespace)
      (let ((start (point))
            (start-col (save-restriction
                         (widen)
                         (- (point) (line-beginning-position)))))
        (cond (count (ignore-errors (dotimes count (forward-sexp))))
              (t (up-list)))
        (end-of-line) ;; get trailing close-parens too, if on same line
        (let* ((end (point))
               (text (filter-buffer-substring start end))
               (lines (sexprw-sexpragon text start-col)))
          (unless lines
            "Non-sexpragonal region")
          (delete-region start end)
          (goto-char init-point) ;; FIXME: redundant?
          (sexprw-emit-sexpragon lines))))))

;; ============================================================

(provide 'sexp-rewrite)
