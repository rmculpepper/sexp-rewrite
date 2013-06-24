;;; sexp-rewrite.el --- pattern-based rewriting of sexp-structured code

;;; Copyright 2013 Ryan Culpepper.
;;; Released under the terms of the GPL version 3 or later;
;;; see the text after `sexprw-legal-notice' for details.

(defconst sexprw-copyright    "Copyright 2013 Ryan Culpepper")
(defconst sexprw-version      "0.01")
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
;; - code cleanup, namespacing, etc
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
;; - generalize (pattern guard template) triples
;;   - add procedure template escape (produces PreOutput)
;;   - to allow PGT triples to occur in patterns
;;     allow recursive processing... 
;;     use cases: cond-else-{if,cond} absorbtion, letrec-to-definitions
;; - package nicely in Emacs idiom
;;   - minor mode ??
;;   - make sure keybindings are added politely
;;   - customization options ??
;; - tweak whitespace handling ??
;; - hook for scan-sexps replacement
;; - hook for scan-whitespace, scan-comments replacements
;; - new spacing rule (in PreOutput): NL*
;;   - acts as NL if *previous template* contained/generated an NL
;;     (will have to figure out how to track that property)
;;     Alternatively, could have LIST, SQLIST just compute & insert
;;     SP vs NL as latent... but that would move interp. of adjacent 
;;     spacing directives into PreOutput pass rather than output pass.
;; - custom var to disable square brackets (use parens instead)
;; - build "tactic apropos"---search by literals in tactic pattern & template

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

(defun sexprw-auto-expression (&optional times)
  "Run the default sexp-rewrite tactics for expressions.
Customizable via the variable `sexpr-auto-expression-tactics'."
  (interactive "p")
  (sexprw-execute-tactics sexprw-auto-expression-tactics times))
(defun sexprw-auto-definition (&optional times)
  "Run the default sexp-rewrite tactics for definitions.
Customizable via the variable `sexpr-auto-definition-tactics'."
  (interactive "p")
  (sexprw-execute-tactics sexprw-auto-definition-tactics times))

(defun sexprw-execute-tactic (tactic-name &optional times0)
  "Read sexprw-rewrite tactic, then try to execute it."
  (interactive
   (list (intern 
          (completing-read "Tactic: "
                           obarray
                           'sexprw-tactic-symbolp
                           t
                           nil
                           'sexprw-tactic-history))
         (prefix-numeric-value current-prefix-arg)))
  (sexprw-execute-tactics (list tactic-name) times0))

(defun sexprw-execute-tactics (tactic-names times0)
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
  (let* ((tactic (sexprw-tactic-value tactic-name)))
    (and (let ((sexprw-current-tactic tactic-name)) ; fluid-let
           (funcall tactic))
         (list tactic-name))))

(defun sexprw-run-tactics-until-success (tactics &optional times0)
  (let ((times times0)
        success
        rused)
    (while (> times 0)
      (setq times (1- times))
      (setq success nil)
      (dolist (tactic tactics)
        (unless success
          (when (sexprw-run-tactic tactic)
            (setq success t)
            (setq rused (cons tactic rused)))))
      (unless success (setq times 0)))
    rused))

;; sexp-rewrite tactic names have property 'sexprw-tactic

(defun sexprw-tactic-symbolp (sym)
  (and (get sym 'sexprw-tactic) t))

(defun sexprw-tactic-value (sym)
  (or (and (symbolp sym) (get sym 'sexprw-tactic))
      (error "Not a sexp-rewrite tactic name: %S" sym)))

(defmacro define-sexprw-tactic (name tactic)
  "Define NAME as a sexp-rewrite tactic specified by TACTIC."
  (unless (and name (symbolp name))
    (error "define-sexprw-tactic: expected symbol for NAME, got: %S" name))
  `(progn (put ',name 'sexprw-tactic (lambda () ,tactic)) ',name))

;; ============================================================
;; Entry points

(defun sexprw-rewrite (pattern template &optional guard)
  (interactive
   (list 
    (read-from-minibuffer "Pattern: " nil nil t 'sexprw-pattern-history)
    (read-from-minibuffer "Template: " nil nil t 'sexprw-template-history)))
  ;; (message "parsed pattern = %S" (desugar-pattern pattern nil 0))
  (sexprw-rewrite/ast (desugar-pattern pattern nil 0)
                      (desugar-pattern template t 0)
                      guard))

(defun sexprw-show-rewrite (pattern template &optional guard)
  (sexprw-show-rewrite/ast (desugar-pattern pattern nil 0)
                           (desugar-pattern template t 0)
                           guard))

(defun sexprw-rewrite/ast (pattern template &optional guard)
  (let ((range (grab-next-sexp/require-pure)))
    (unless range (error "Not at start of sexp"))
    (let ((start (nth 1 range))
          (end (nth 2 range)))
      (goto-char start)
      (save-excursion
        (let* ((replacement (sexprw-show-rewrite/ast pattern template guard))
               (replacement-length (length replacement)))
          (and replacement
               (progn
                 (delete-and-extract-region start end)
                 (insert replacement)
                 (indent-region start (+ start (length replacement)))
                 t)))))))

(defun sexprw-show-rewrite/ast (pattern template &optional guard)
  ;; (message "pattern = %S" pattern)
  (save-excursion
    (let ((env (sexprw-match pattern)))
      ;; (message "env = %S" env)
      (and env
           (sexprw-check-nonlinear-patterns (car env))
           (let ((env* (if guard (funcall guard (car env)) env)))
             ;; (message "guarded env = %S" env*)
             (and (or env*
                      (sexprw-fail `(guard env= ,env)))
                  (let ((preoutput
                         (condition-case error-info
                             (sexprw-template template (car env*))
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
                           output)))))))))

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
            (unless (equal entry1 entry2)
              (sexprw-fail `(nonlinear-pvar ,key1 env= ,env0))
              (setq ok nil))))))
    ok))

;; ============================================================
;; Debugging and diagnostics

(defvar sexprw-current-tactic nil
  "Name of currently executing tactic.")

(defvar sexprw-failure-info nil
  "Information about last tactic failure(s).")

(defun sexprw-fail (info)
  (push (cons sexprw-current-tactic (cons (point) info)) sexprw-failure-info)
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
;; Pretty patterns and templates

;; PP ::= symbol         ~ (quote symbol)
;;      | $name          ~ (VAR $name SYM)     ; sigil is part of pvar name
;;      | %name          ~ (VAR %name SEXP)
;;      | %%name         ~ (VAR %%name REST n) ; n is # patterns that follow
;;      | (PP*)          ~ (LIST P*)
;;      | (!SPLICE P*)   ~ (SPLICE P*)
;;      | (!REP PP)      ~ (REP P 0)
;;      | PP ...         ~ (REP P n)           ; n is # patterns that follow

;; PT ::= like PP, with following additions and replacements:
;;      | (!SQ PT*)      ~ (SQLIST T*)
;;      | !NL            ~ (NL)
;;      | !SP            ~ (SP)
;;      | (!REP PT vars) ~ (REP T vars)
;;      | PT ...         ~ (REP T nil)         ; vars=nil means "auto"

(defun desugar-pattern (pretty template upto)
  (cond ((null pretty)
         '(LIST))
        ((symbolp pretty)
         (let ((name (symbol-name pretty)))
           (cond ((and template (eq pretty '!NL))
                  '(NL))
                 ((and template (eq pretty '!SP))
                  '(SP))
                 ((eq pretty '...)
                  (error "Misplaced ellipses: %S" pretty))
                 ((string-match "^[!]" name)
                  (error "Bad symbol in %s: %S"
                         (if template "template" "pattern")
                         pretty))
                 ((string-match "^[$][[:alpha:]].*" name)
                  `(VAR ,pretty ,@(if template '() '(SYM))))
                 ((string-match "^[%][[:alpha:]].*" name)
                  `(VAR ,pretty ,@(if template '() '(SEXP))))
                 ((string-match "^[%][%][[:alpha:]].*" name)
                  (unless (or upto template)
                    (t (error "Patterns of unknown size follow %S" pretty)))
                  `(VAR ,pretty ,@(if template '() `(REST ,upto))))
                 (t `(quote ,pretty)))))
        ((not (consp pretty))
         (error "Bad %s: %s" (if template "template" "pattern") pretty))
        ((eq (car pretty) '!SPLICE)
         (cons 'SPLICE (desugar-pattern-list (cdr pretty) template upto)))
        ((eq (car pretty) '!SQ)
         (if template
             (cons 'SQLIST (desugar-pattern-list (cdr pretty) template upto))
             (error "Bad pattern (!SQ not allowed): %S" pretty)))
        ((eq (car pretty) '!REP)
         (list 'REP
               (desugar-pattern (nth 1 pretty) template upto)
               (cond (template
                      (nth 2 pretty))
                     ;; pattern
                     ((consp (nthcdr 2 pretty))
                      (nth 2 pretty))
                     (t
                      (unless upto
                        (error "Patterns of unknown size follow !REP pattern: %S"
                               pretty))
                      upto))))
        (t ; list
         (cons 'LIST (desugar-pattern-list pretty template 0)))))

(defun desugar-pattern-list (pretty template upto)
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
               (let ((pp1 (desugar-pattern p1 template upto)))
                 (when dots
                   (setq dots nil)
                   (setq pp1 (list 'REP pp1 (if template nil upto))))
                 (setq upto (+ upto (sexprw-pattern-min-size pp1)))
                 (setq accum (cons pp1 accum)))))))
    (when dots (error "Misplaced dots at beginning of pattern: %S" pretty))
    accum))

(defun sexprw-pattern-min-size (p)
  (cond ((and (eq (car p) 'VAR)
              (eq (nth 2 p) 'REST))
         0)
        ((memq (car p) '(quote VAR LIST))  ; note: not (VAR _ REST _)
         1)
        ((eq (car p) 'SPLICE)
         (apply #'+ (mapcar #'sexprw-pattern-min-size (cdr p))))
        (t 0)))

;; ============================================================
;; Patterns

;; P ::= (LIST P*)
;;     | (SPLICE P*)
;;     | (quote symbol)
;;     | (VAR symbol VariableKind)
;;     | (REP P n)
;;
;; VariableKind ::= SYM       ; symbol
;;                | PURE-SEXP ; next sexp, require no preceding comments
;;                | SEXP      ; next sexp and preceding comments
;;                | REST n    ; rest of enclosing sexp, stopping 'n' sexps
;;                            ; before end; if 0, gets trailing comments too
;;
;; Matching builds an alist mapping pvar symbols to EnvValue
;; EnvValue ::= (list 'atom string)              ; representing symbol
;;            | (list 'block string one-line?)   ; representing SEXP, REST
;;            | (list 'rep EnvValue)             ; representing depth>0 list
;;
;; (REP P n), like (VAR name REST n), stops 'n' sexps before end

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
  "Matches the sexp starting at point against PATTERN, returning an
\(list alist) mapping the pattern variables of PATTERN to
fragments, or nil on failure.  Advances point to end of matched
term(s)."
  ;; (message "matching (%S): %S" (point) pattern)
  (cond ((not (consp pattern))
         (error "Bad pattern: %s" pattern))
        ((eq (car pattern) 'quote)
         ;; Note: grabs pure-sexp, checks contains symbol
         (let ((next (grab-next-sexp/require-pure)))
           (and (or next
                    (sexprw-fail `(match quote pure-sexp)))
                (let ((pure-text (car next)))
                  (and (or (string-match sexprw-pure-atom-re pure-text)
                           (sexprw-fail `(match quote is-symbol)))
                       (or (equal pure-text (symbol-name (cadr pattern)))
                           (sexprw-fail
                            `(match quote equal
                                    ,(symbol-name (cadr pattern)))))
                       (list nil))))))
        ((eq (car pattern) 'VAR)
         (sexprw-match-var (nth 1 pattern) (nth 2 pattern) (nthcdr 3 pattern)))
        ((eq (car pattern) 'LIST)
         (sexprw-match-list (cdr pattern)))
        ((eq (car pattern) 'SPLICE)
         (sexprw-match-list-contents (cdr pattern)))
        ((eq (car pattern) 'REP)
         (sexprw-match-rep (nth 1 pattern) (nth 2 pattern)))
        (t (error "Bad pattern: %S" pattern))))

(defun sexprw-match-var (pvar kind args)
  (cond ((eq kind 'SYM)
         ;; Note: grabs pure-sexp, checks contains symbol
         (let ((next (grab-next-sexp/require-pure)))
           (and (or next
                    (sexprw-fail `(match var sym grab)))
                (let ((pure-text (car next)))
                  (and (or (string-match sexprw-pure-atom-re pure-text)
                           (sexprw-fail `(match var sym equal)))
                       (list (list (cons pvar (list 'atom pure-text)))))))))
        ((eq kind 'PURE-SEXP)
         (let ((next (grab-next-sexp/require-pure)))
           (and (or next
                    (sexprw-fail `(match var pure-sexp grab)))
                (let ((pure-text (car next))
                      (same-line (= (line-number-at-pos (nth 1 next))
                                    (line-number-at-pos (nth 2 next)))))
                  (list (list (cons pvar (list 'block pure-text
                                               same-line))))))))
        ((eq kind 'SEXP)
         (let ((next (grab-next-impure-sexp)))
           (and (or next
                    (sexprw-fail `(match var sexp grab)))
                (let ((impure-text (car next))
                      (same-line   (= (line-number-at-pos (nth 1 next))
                                      (line-number-at-pos (nth 2 next)))))
                  (list (list (cons pvar (list 'block impure-text
                                               same-line))))))))
        ((eq kind 'REST)
         (sexprw-skip-whitespace)
         (let ((init-point (point)))
           (and (or (skip-forward-to-n-sexps-before-end (car args))
                    (sexprw-fail `(match var rest skip ,(car args))))
                (list
                 (list (cons pvar
                             (list 'block
                                   (filter-buffer-substring init-point (point))
                                   (line-number-at-pos init-point)
                                   (line-number-at-pos (point)))))))))
        (t (error "Bad pattern variable kind for pvar '%s': %s" pvar kind))))

;; returns t on success, nil if fewer than n sexps before end
(defun skip-forward-to-n-sexps-before-end (n)
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
  (let ((next (grab-next-sexp/require-pure)))
    (and (or next
             (sexprw-fail `(match-list grab)))
         (member (substring (car next) 0 1) '("(" "[" "{"))
         ;; narrow to just after start, just before end
         (let ((result
                (save-excursion ; FIXME: necessary?
                  (save-restriction
                    (goto-char (1+ (nth 1 next)))
                    (narrow-to-region (1+ (nth 1 next)) (1- (nth 2 next)))
                    (sexprw-match-list-contents inners)))))
           ;; save-excursion already resets to end of list
           ;; (when result (goto-char (nth 2 next)))
           result))))

(defun sexprw-match-list-contents (inners)
  (let ((accum (list '()))) ; nil or (list alist)
    (dolist (inner inners)
      (when accum
        (let ((inner-result (sexprw-match inner)))
          (setq accum (and inner-result
                           (list (append (car inner-result) (car accum))))))))
    (and accum
         (or (looking-at (concat sexprw-all-whitespace-re "\\'"))
             (sexprw-fail `(match-list end check-whitespace)))
         accum)))

(defun sexprw-match-rep (inner upto)
  (let ((raccum '())
        (proceed (point)))
    (save-restriction
      (skip-forward-to-n-sexps-before-end upto)
      (narrow-to-region proceed (point))
      (while proceed
        (goto-char proceed)
        (save-excursion
          (let ((next-result (sexprw-match inner)))
            (cond (next-result
                   (setq raccum (cons (car next-result) raccum))
                   (setq proceed (point)))
                  (t
                   (setq proceed nil))))))
      (list (sexprw-reverse-merge-alists inner raccum)))))

(defun sexprw-reverse-merge-alists (inner alists)
  (cond ((consp alists)
         (let ((alist1 (car alists))
               (accum '()))
           (dolist (entry alist1)
             (let ((key (car entry))
                   (values '()))
               (dolist (alist alists)
                 (setq values (cons (cdr (assq key alist)) values)))
               (setq accum (cons (cons key (cons 'rep values)) accum))))
           (reverse accum)))
        (t
         ;; Need to build alist of pattern variables of inner.
         (let (accum '())
           (dolist (pvar (sexprw-pattern-variables inner '()))
             (setq accum (cons (cons pvar '(rep)) accum)))
           (reverse accum)))))

(defun sexprw-pattern-variables (pattern onto)
  ;; Accept templates too
  (cond ((eq (car pattern) 'VAR)
         (cons (nth 1 pattern) onto))
        ((memq (car pattern) '(LIST SPLICE SQLIST))
         (dolist (inner (cdr pattern))
           (setq onto (sexprw-pattern-variables inner onto)))
         onto)
        ((eq (car pattern) 'REP)
         (sexprw-pattern-variables (nth 1 pattern) onto))
        ((memq (car pattern) '(quote SP NL))
         onto)
        (t (error "Bad pattern: %S" pattern))))

;; ----

(defun grab-next-impure-sexp ()
  "Returns (list IMPURE-TEXT WS-POINT END-POINT) or nil.
IMPURE-TEXT is text from WS-POINT to END-POINT.
WS-POINT is the location of the first non-whitespace character.
Advances point to end of sexp."
  (grab-next-sexp nil nil))

(defun grab-next-pure-sexp ()
  "Returns (list PURE-TEXT START-POINT END-POINT) or nil.
PURE-TEXT is text from START-POINT to END-POINT.
START-POINT is the location of the first non-whitespace character.
Advances point to end of sexp."
  (grab-next-sexp t nil))

(defun grab-next-sexp/require-pure ()
  "Returns (list PURE-TEXT START-POINT END-POINT) or nil.
PURE-TEXT is text from START-POINT to END-POINT.  START-POINT is
the location of the first non-whitespace character, which must
also be the start of the sexp.  Advances point to end of sexp."
  (grab-next-sexp t t))

(defun grab-next-sexp (pure require-pure)
  "Returns (list TEXT START-POINT END-POINT) or nil.
TEXT is text from START-POINT to END-POINT.  If PURE is non-nil,
then START-POINT is taken as the start of the sexp; otherwise, it
is the first non-whitespace character.  If REQUIRE-PURE is
non-nil, then there must be no non-whitespace characters before
the start of the sexp, or else nil is returned.  Advances point
to end of sexp."
  (let ((result (grab-next-sexp-range)))
    (and result
         (or (not require-pure) (equal (nth 1 result) (nth 2 result)))
         (let ((start (if pure (nth 2 result) (nth 1 result)))
               (end (nth 3 result)))
           (goto-char end)
           (list (filter-buffer-substring start end)
                 start
                 end)))))

(defun grab-next-sexp-range ()
  ;; FIXME/BUG: backwards scan loses things like quote prefix, 
  ;; can lead to treating "'x" as atomic sexp (shouldn't be).
  ;; Maybe add custom comment handling to avoid backwards scan?
  "Returns (list INIT-POINT WS-POINT START-POINT END-POINT) or nil.
INIT-POINT is where point started. WS-POINT is the location of
the first non-whitespace character. START-POINT is where the sexp
starts.  END-POINT is where the sexp ends.  Does not change
point."
  (condition-case error-info
      (save-excursion
        (let ((init-point (point)))
          (sexprw-skip-whitespace)
          (let* ((ws-point (point))
                 (end-point (scan-sexps ws-point 1))
                 (start-point (and end-point (scan-sexps end-point -1))))
            ;; scan-sexps signals error if EOF inside parens,
            ;; returns nil if EOF no sexp found
            (cond ((and start-point
                        (< start-point end-point))
                   (list init-point ws-point start-point end-point))
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
          (cond ((eq (car item) 'atom)
                 (when (equal (cadr item) ".")
                   (setq failed t)))
                ((eq (car item) 'rep)
                 (setq worklist (append (cdr item) worklist)))
                (t
                 (error "Non-atom value for pvar '%s': %S" pvar item))))))
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
;;     | (NL)            ; latent newline
;;     | (REP vars T*)   ; NOTE: splicing repetition, to allow eg
;;     |                 ; (REP <vars> expr NL) vars is pvars to map over
;;
;; PreOutput = (treeof (U string 'SP 'NL 'NONE))
;; Interpret PreOutput left to right; *last* spacing symbol to occur wins.

(defun sexprw-template (template env)
  "Interprets TEMPLATE using the pattern variables of ENV.
Returns a list of strings and latent spacing symbols ('SP and 'NL)."
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
        ((eq (car template) 'LIST)
         (list "("
               (sexprw-template-list-contents (cdr template) env)
               'NONE
               ")"
               'SP))
        ((eq (car template) 'SQLIST)
         (list "["
               (sexprw-template-list-contents (cdr template) env)
               'NONE
               "]"
               'SP))
        ((eq (car template) 'SPLICE)
         (sexprw-template-list-contents (cdr template) env))
        ((eq (car template) 'SP)
         'SP)
        ((eq (car template) 'NL)
         'NL)
        ((eq (car template) 'REP)
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
    (unless vars (error "No repetition vars for REP: ~S" template))
    (let* ((lengths (mapcar #'length vals))
           (length1 (car lengths)))
      (dolist (l lengths)
        (unless (= l length1)
          (signal 'template-error 'ellipsis-count-mismatch)))
      (let ((raccum '()))
        (dotimes (_i length1)
          (let* ((extenv+vals (split/extend-env vars vals env))
                 (extenv (car extenv+vals)))
            (setq vals (cdr extenv+vals))
            (setq raccum 
                  (cons (sexprw-template inner extenv)
                        raccum))))
        (reverse raccum)))))

(defun split/extend-env (vars vals env)
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
      (cond ((and (consp value) (eq (car value) 'atom))
             (list (cadr value) 'SP))
            ((and (consp value) (eq (car value) 'block))
             (list (if (nth 2 value) nil 'NL) ; FIXME: tweak?
                   (nth 1 value)
                   (if (nth 2 value) 'SP 'NL)))
            ((and (consp value) (eq (car value) 'rep))
             (error "Depth error for pvar '%s'; value is: %S" pvar value))
            (t (error "Bad pvar value for pvar '%s': %s" pvar value))))))

(defun sexprw-template-list-contents (inners env)
  ;; We don't add inter-element spacing here; 
  ;; each element should add its own trailing spacing.
  (let ((accum (list '()))) ; nil or (list PreOutput)
    (dolist (inner inners)
      (setq accum (cons accum (sexprw-template inner env))))
    accum))

(defun sexprw-output (pre)
  (let* ((result (sexprw-output* pre nil 'NONE))
         (raccum (car result))
         (latent (cdr result)))
    (apply #'concat (reverse raccum))))

(defun sexprw-output* (pre raccum latent)
  (cond ((consp pre)
         (let ((result (sexprw-output* (car pre) raccum latent)))
           (sexprw-output* (cdr pre) (car result) (cdr result))))
        ((stringp pre)
         (let* ((raccum (cons (sexprw-spacing-to-string latent) raccum))
                (raccum (cons pre raccum)))
           (cons raccum 'NONE)))
        ((null pre)
         (cons raccum latent))
        ((symbolp pre)
         (cons raccum pre))
        (t
         (error "Bad pre-output: %S" pre))))

(defun sexprw-spacing-to-string (spacing)
  (cond ((eq spacing 'NL) "\n")
        ((eq spacing 'SP) " ")
        ((eq spacing 'NONE) "")
        (t (error "Bad spacing: %S" spacing))))


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
           (goto-char (1+ (point)))
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
                               'sexpr-pattern-history)))
  (sexprw-search-pattern/ast (desugar-pattern pattern nil 0)))

(defun sexprw-search-pattern/ast (pattern)
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

(provide 'sexp-rewrite)
