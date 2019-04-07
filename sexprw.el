;;; sexprw.el --- pattern-based rewriting of sexp-structured code  -*- lexical-binding:t -*-

;; Copyright 2013-2019 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later;
;; see the text after `sexprw-legal-notice' for details.

;; Version: 0.04

(defconst sexprw-copyright    "Copyright 2013-2019 Ryan Culpepper")
(defconst sexprw-version      "0.04")
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

;;; Commentary:

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
;; - put rewrite rules in "bundles", and enable different "bundles" for
;;   different major modes (scheme-mode, racket-mode, emacs-lisp-mode, ...)
;; - current-column vs (save-restriction (widen) (- (point) (beginning-of-line))) ??
;; - idiomatic prefix-argument behavior

;; long term
;; - port to DrRacket
;; - use DrRacket semantic info (eg, freevars) for safety

;; ============================================================
;; Misc notes

;; Matching functions, etc return nil on failure, only raise error on
;; bad input (illegal pattern, etc).

(require 'sexprw-text)
(require 'sexprw-core)
(require 'sexprw-desugar)
(require 'sexprw-rewrite)
(require 'sexprw-edit)
(require 'sexprw-search)

;; ============================================================
;; Keybindings

;;; Code:

(defvar sexprw-mode-map
  (let ((mainmap (make-sparse-keymap))
        (map (make-sparse-keymap)))
    (define-key mainmap (kbd "C-c C-s") map)

    (define-key map "e" 'sexprw-auto-expression)
    (define-key map "d" 'sexprw-auto-definition)
    (define-key map "x" 'sexprw-execute-tactic)
    (define-key map "s" 'sexprw-search-pattern)
    (define-key map "i" 'sexprw-search-rewrite)
    (define-key map "[" 'sexprw-squarify)
    (define-key map "(" 'sexprw-roundify)

    (define-key map "k" 'sexprw-kill-next-sexpagon-sexp)
    (define-key map "w" 'sexprw-kill-sexpagon-region)
    (define-key map "y" 'sexprw-yank-sexpagon)

    (define-key map (kbd "M-SPC") 'sexprw-collapse-space/move-sexps)
    (define-key map [tab] 'sexprw-indent-rigidly)

    (define-key map (kbd "r e") 'sexprw-repeat-auto-expression)
    (define-key map (kbd "r d") 'sexprw-repeat-auto-definition)
    mainmap))

(defvar sexprw-auto-expression-tactics nil
  "List of tactics tried by `sexprw-auto-expression'.")
(defvar sexprw-auto-definition-tactics nil
  "List of tactics tried by `sexprw-auto-definition'.")

(defgroup sexprw nil
  "Customization options for sexp-rewrite (sexprw)."
  :group 'scheme)

(defcustom sexprw-disabled-auto-tactics nil
  "Tactics that should not be run automatically.
Affects only `sexprw-auto-expression' and `sexprw-auto-definition';
disabled tactics can still be run via `sexprw-execute-tactic', etc."
  :type '(repeat symbol))

;;;###autoload
(define-minor-mode sexprw-mode
  "Minor mode for pattern-based rewrite of sexp-structured code.
\\{sexprw-mode-map}"
  ;; Implicitly activates sexprw-mode-map when enabled.
  :init-value nil)

;; FIXME: This should likely be in an emacs-lisp-rewrite.el with corresponding
;; rewrite rules.
;;;###autoload
(add-hook 'emacs-lisp-mode-hook #'sexprw-mode)
;;;###autoload
(add-hook 'scheme-mode-hook #'sexprw-mode)

(defun sexprw-disable-tactic (tactic-name)
  (interactive
   (list (sexprw-read-tactic-from-minibuffer)))
  (push tactic-name sexprw-disabled-auto-tactics))

(defun sexprw-enable-tactic (tactic-name)
  (interactive
   (list (sexprw-read-tactic-from-minibuffer)))
  (setq sexprw-disabled-auto-tactics
        (delete tactic-name sexprw-disabled-auto-tactics)))

;; ============================================================
;; Debugging and diagnostics

(defun sexprw-show-failure-info ()
  (interactive)
  (message "%S" sexprw-failure-info))

(define-error 'sexprw-template-error "Error instantiating template")

;; ============================================================
;; Running tactics

(defun sexprw-auto-expression (&optional times)
  "Run the default sexp-rewrite tactics for expressions.
Customizable via the variable `sexprw-auto-expression-tactics'."
  (interactive "p")
  (sexprw-execute-tactics sexprw-auto-expression-tactics
                          times sexprw-disabled-auto-tactics))
(defun sexprw-auto-definition (&optional times)
  "Run the default sexp-rewrite tactics for definitions.
Customizable via the variable `sexprw-auto-definition-tactics'."
  (interactive "p")
  (sexprw-execute-tactics sexprw-auto-definition-tactics
                          times sexprw-disabled-auto-tactics))

(defun sexprw-repeat-auto-expression ()
  "Repeatedly run the default sexp-rewrite tactics for expressions,
until no tactic is applicable or to 100 iterations.
Customizable via the variable `sexprw-auto-expression-tactics'."
  (interactive)
  (sexprw-auto-expression 100))
(defun sexprw-repeat-auto-definition ()
  "Repeatedly run the default sexp-rewrite tactics for definitions,
until no tactic is applicable or to 100 iterations.
Customizable via the variable `sexprw-auto-definition-tactics'."
  (interactive)
  (sexprw-auto-definition 100))

(defun sexprw-execute-tactic (tactic-name &optional times0)
  "Read sexprw-rewrite tactic, then try to execute it."
  (interactive
   (list (sexprw-read-tactic-from-minibuffer)
         (prefix-numeric-value current-prefix-arg)))
  (sexprw-execute-tactics (list tactic-name) times0 nil))

(defun sexprw-execute-tactics (tactic-names times0 disabled)
  (setq sexprw-failure-info nil)
  (let ((rused (sexprw-run-tactics-until-success tactic-names times0 disabled)))
    (cond ((consp rused)
           (cond ((= (length rused) 1)
                  (message "Applied tactic %s" (car rused)))
                 (t (message "Applied tactics: %s" (reverse rused)))))
          (t
           (cond ((= (length tactic-names) 1)
                  (message "Tactic %s not applicable" (car tactic-names)))
                 (t (message "No applicable tactic")))))))

;; ============================================================

(provide 'sexprw)
;;; sexprw.el ends here.
