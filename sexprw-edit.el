;;; sexprw-edit.el --- misc editing commands  -*- lexical-binding:t -*-

;; Copyright 2013-2019 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later;
;; see sexprw.el for details.

(require 'sexprw-text)

;; ============================================================
;; Convert brackets

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

(defun sexprw-open-bracket-re (from)
  ;; (concat "[[:space:]]*" (regexp-quote from))
  ;; (concat "\\s-*" (regexp-quote from))  ; doesn't get newlines
  (concat "[[:space:]\n]*" (regexp-quote from)))

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
;; Sexpagon functions

(defun sexprw-kill-next-sexpagon-sexp ()
  "Kills the sexp at point, preserving relative indentation.
The sexp must be a sexpagon. Whitespace is removed from lines
after the first so the sexp will be properly indented when
`yank'ed at column 0 or yanked via `sexprw-yank-sexpagon'."
  (interactive)
  (let* ((init-point (point))
         (next (sexprw-grab-next-sexp-range)))
    (unless next
      (error "No sexp at point"))
    (let* ((start (nth 1 next))
           (start-col (save-excursion
                        (save-restriction
                          (widen)
                          (goto-char start)
                          (- start (line-beginning-position)))))
           (end (nth 3 next))
           (lines (sexprw-sexpagon (filter-buffer-substring start end) start-col)))
      (unless lines
        (error "Non-sexpagonal sexp at point"))
      (let ((text (mapconcat 'identity lines "\n")))
        (delete-and-extract-region init-point end)
        (kill-new text)))))

(defun sexprw-kill-sexpagon-region (start end)
  "Kills from START to END, preserving relative indentation.
The region must be a sexpagon. Whitespace is removed from lines
after the first so the sexp will be properly indented when
`yank'ed at column 0 or yanked via `sexprw-yank-sexpagon'."
  (interactive "r")
  (let ((text (filter-buffer-substring start end))
        (start-col (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char start)
                       (- start (line-beginning-position))))))
    (let ((lines (sexprw-sexpagon text start-col)))
      (unless lines
        (error "Non-sexpagonal region"))
      (let ((text (mapconcat 'identity lines "\n")))
        (delete-and-extract-region start end)
        (kill-new text)))))

(defun sexprw-yank-sexpagon ()
  "Yanks text, preserving relative indentation of multi-line text.
Whitespace is added to lines after the first so each line starts
at the same column as the first line."
  (interactive)
  (let ((text (current-kill 0)))
    (unless text
      (error "No text in kill ring"))
    (sexprw-emit-sexpagon (split-string text "[\n]" nil))))

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
        (cond (count (ignore-errors (dotimes (_i count) (forward-sexp))))
              (t (up-list)))
        (end-of-line) ;; get trailing close-parens too, if on same line
        (let* ((end (point))
               (text (filter-buffer-substring start end))
               (lines (sexprw-sexpagon text start-col)))
          (unless lines
            (error "Non-sexpagonal region"))
          (delete-region start end)
          (goto-char init-point) ;; FIXME: redundant?
          (sexprw-emit-sexpagon lines))))))

;; ============================================================
;; Move region rigidly

(defun sexprw-indent-rigidly (count)
  "Set the active region and call `sexprw-indent-region-rigidly'.
The region is set according to the following rules:

- If a region is already active, that region is used.
- If the prefix argument is a positive integer COUNT, then the
  region consists of the next COUNT S-expressions.
- Otherwise, the region extends to the end of the enclosing
  S-expression (if there is one) or to the end of the buffer."
  (interactive "P")
  (when (consp count) (setq count (car count)))
  (unless (integerp count) (setq count nil))
  (cond ((region-active-p)
         (sexprw-indent-region-rigidly))
        ((and (integerp count) (> count 0))
         (sexprw-skip-whitespace)
         (let ((end (save-excursion
                      (ignore-errors (dotimes (_i count) (forward-sexp)))
                      (point))))
           (push-mark end t t)
           (sexprw-indent-region-rigidly)))
        (t
         (sexprw-skip-whitespace)
         (let ((end (save-excursion
                      (or (ignore-errors (up-list) (point))
                          (point-max)))))
           (push-mark end t t)
           (sexprw-indent-region-rigidly)))))

(defvar sexprw-indent-rigidly-map
  (let ((map (make-sparse-keymap)))
    (define-key map [left]   'sexprw-indent-rigidly-left)
    (define-key map [right]  'sexprw-indent-rigidly-right)
    (define-key map [up]     'sexprw-indent-rigidly-up)
    (define-key map [down]   'sexprw-indent-rigidly-down)
    (define-key map [return] 'sexprw-indent-rigidly-newline)
    (define-key map [tab]    'sexprw-indent-rigidly-indent)
    map)
  "Transient keymap for adjusting indentation interactively.
It is activated by calling `sexprw-indent-region-rigidly' interactively.")

(defun sexprw-indent-region-rigidly ()
  "Like `indent-rigidly' but also moves the selected segment of
the first line and can move the region vertically as well as
horizontally."
  (interactive)
  (message
   (substitute-command-keys
    "Move region with \\<sexprw-indent-rigidly-map>\\[sexprw-indent-rigidly-left], \\[sexprw-indent-rigidly-right], \\[sexprw-indent-rigidly-up], \\[sexprw-indent-rigidly-down], \\[sexprw-indent-rigidly-newline], or \\[sexprw-indent-rigidly-indent]."))
  ;; FIXME: `undo' gives "undo in region" warning, bad behavior! I tried passing
  ;; `deactivate-mark' as the on-exit callback, but it didn't seem to help.
  (set-transient-map sexprw-indent-rigidly-map t))

(defun sexprw--indent-rigidly-pop-undo ()
  (and (memq last-command '(sexprw-indent-rigidly-left
                            sexprw-indent-rigidly-right
                            sexprw-indent-rigidly-up
                            sexprw-indent-rigidly-down
                            sexprw-indent-rigidly-newline
                            sexprw-indent-rigidly-indent))
       (consp buffer-undo-list)
       (eq (car buffer-undo-list) nil)
       (pop buffer-undo-list)))

(defun sexprw--region-excursion (proc)
  "Like save-excursion, but keeps the region over the same bits
of text. IIUC, save-excursion uses markers with the wrong
insertion mode, so inserted whitespace would become part of the
region. That can be fixed with insert-before-markers, but that
won't work for newline-and-indent, etc.  Also does additional
transient mode stuff (undos, keep mark active)."
  (when (region-active-p)
    (sexprw--indent-rigidly-pop-undo)
    (let ((am (copy-marker (region-beginning) t))
          (bm (copy-marker (region-end))))
      (save-excursion
        (goto-char am)
        (funcall proc am bm))
      (goto-char am)
      (set-mark bm)
      (move-marker am nil)
      (move-marker bm nil)))
  ;; Keep the active region in transient mode.
  (when (eq (cadr overriding-terminal-local-map) sexprw-indent-rigidly-map)
    (setq deactivate-mark nil)))

(defun sexprw-indent-rigidly-right ()
  "Move the active region right by one space."
  (interactive)
  (sexprw--region-excursion
   (lambda (_beg end)
     (insert " ")
     (forward-line 1)
     (when (< (point) end)
       (indent-rigidly (point) end 1)))))

(defun sexprw-indent-rigidly-left ()
  "Move the active region left by one space. If there is no
horizontal whitespace immediately before the region, there is no
effect."
  ;; FIXME: preserve indentation
  ;; FIXME: use current-column, indent-to to preserve tabs?
  (interactive)
  (sexprw--region-excursion
   (lambda (beg end)
     (when (looking-back " " (1- beg))
       (delete-region (1- beg) beg)
       (goto-char beg)
       (forward-line 1)
       (when (< (point) end)
         (indent-rigidly (point) end -1))))))

(defun sexprw-indent-rigidly-down ()
  "Move the active region down by one line on the same column."
  (interactive)
  (sexprw--region-excursion
   (lambda (_beg _end)
     (let ((col (current-column)))
       (delete-horizontal-space t)
       (newline)
       (indent-to col)))))

(defun sexprw-indent-rigidly-up ()
  "Move the active region up by one line. If there are
non-whitespace characters on the line where the region starts,
this command has no effect. Otherwise, the region is moved up on
the same column or on the first column after all non-whitespace
characters."
  (interactive)
  (sexprw--region-excursion
   (lambda (beg end)
     (let* ((col (current-column))
            (up (save-excursion
                  (forward-line -1)
                  (move-to-column col t)
                  (point)))
            (line-start (line-beginning-position)))
       (skip-chars-backward "[:space:]\n" up)
       (let ((pos (point))
             (col2 (current-column)))
         (when (< pos line-start)
           (delete-region pos beg)
           (forward-line 1)
           (when (< (point) end)
             (indent-rigidly (point) end (- col2 col)))))))))

(defun sexprw-indent-rigidly-indent ()
  "Move the active region by indenting (using
`indent-according-to-mode'), and preserve the relative
indentation of the subsequent lines."
  (interactive)
  (sexprw--region-excursion
   (lambda (_beg end)
     (let ((col (current-column)))
       (when (looking-back "^[\t ]*" (save-excursion (beginning-of-line) (point)))
         (indent-according-to-mode)
         (let ((col2 (current-column)))
           (forward-line 1)
           (when (< (point) end)
             (indent-rigidly (point) end (- col2 col)))))))))

(defun sexprw-indent-rigidly-newline ()
  "Move the active region down one line and indent the first
line (using `newline-and-indent'), and preserve the relative
indentation of the subsequent lines."
  (interactive)
  (sexprw--region-excursion
   (lambda (_beg end)
     (let ((col1 (current-column)))
       (newline-and-indent)
       (let ((col2 (current-column)))
         (forward-line 1)
         (when (< (point) end)
           (indent-rigidly (point) end (- col2 col1))))))))

;; ============================================================

(provide 'sexprw-edit)
;;; sexprw-edit.el ends here.
