;;; sexprw-search.el --- pattern-based search  -*- lexical-binding:t -*-

;; Copyright 2013-2019 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later;
;; see sexprw.el for details.

(require 'sexprw-text)
(require 'sexprw-core)
(require 'sexprw-desugar)
(require 'sexprw-rewrite)

;; ============================================================
;; Search with patterns

(defun sexprw-search-pattern (pattern)
  "Search forward for sexp matching PATTERN.
On success, moves point and saves old point to mark."
  (interactive
   (list (read-from-minibuffer "Search pattern: " nil nil t
                               'sexprw-pattern-history)))
  (let ((sexprw-current-operation 'search)) ;; fluid-let
    (setq sexprw-failure-info nil)
    (let ((init-point (point))
          (result (sexprw-search-pattern/ast (sexprw-desugar-pattern pattern nil))))
      (cond (result
             (push-mark init-point)
             (message "Pattern found; mark saved where search started"))
            (t
             (goto-char init-point)
             (message "Pattern not found"))))))

(defun sexprw-search-pattern/ast (pattern)
  "Search forward for match to given core PATTERN.
Moves point regardless of success or failure."
  (let ((success nil)
        (continue t))
    (while continue
      (setq continue nil)
      (sexprw-skip-whitespace)
      (let ((result (save-excursion (sexprw-match pattern))))
        (cond (result
               (setq success result))
              (t
               (setq continue (sexprw-move-forward))))))
    success))

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
;; Search and Rewrite

(defun sexprw-search-rewrite (pattern template)
  "Search forward for PATTERN; if found, rewrite to TEMPLATE."
  (interactive
   (list (read-from-minibuffer "Search pattern: " nil nil t
                               'sexprw-pattern-history)
         (read-from-minibuffer "Rewrite template: " nil nil t
                               'sexprw-template-history)))
  (let ((sexprw-current-operation 'search)) ;; fluid-let
    (setq sexprw-failure-info nil)
    (let ((init-point (point))
          (result (sexprw-search-pattern/ast (sexprw-desugar-pattern pattern nil))))
      (cond (result
             (push-mark init-point)
             (message "Pattern found; mark saved where search started")
             (sexprw-rewrite pattern template))
            (t
             (goto-char init-point)
             (message "Pattern not found"))))))

;; ============================================================

(provide 'sexprw-search)
;;; sexprw-search.el ends here.
