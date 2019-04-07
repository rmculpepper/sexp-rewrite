;;; sexprw-text.el --- text and buffer functions  -*- lexical-binding:t -*-

;; Copyright 2013-2019 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later;
;; see sexprw.el for details.

;; ============================================================
;; Blocks

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

(defun sexprw-block-sexpagon (block)
  (let* ((text (sexprw-block-text block))
         (start-col (sexprw-block-start-column block)))
    (sexprw-sexpagon text start-col)))

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
  (condition-case _error-info
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
;; Sexpagons

;; A "sexpagon" is the shape of a well-formatted sexp:

;;     (----------+
;;     |          |
;;     |      +---+
;;     +------)

;; There must be no non-whitespace characters to the left of the open
;; paren's column from the second line to the last. Well-formatted
;; Lisp/Scheme/Racket code is nearly always sexpagonal, with the
;; occasional exception of multi-line string literals.

(defun sexprw-sexpagon (text start-col)
  "If TEXT is a sexpagon, remove its indentation; else return nil.

Given TEXT, a string whose first line originally started at
column START-COL, return the (non-empty) list of strings
representing the lines with the START-COL spaces of indentation
removed. If some line has less than START-COL spaces of
indentation (that is, TEXT is not a sexpagon), the function
returns nil."
  ;; FIXME: allow tab characters in indentation!
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

(defun sexprw-emit-sexpagon (lines)
  "Write LINES (a non-empty list of strings) as a sexpagon."
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
;; Emit Output

;; Output = (listof (U string 'NL (cons 'SEXPAGON listofstring)))

(defun sexprw-emit (output)
  (while output
    (let ((fragment (car output)))
      (setq output (cdr output))
      (cond ((eq fragment 'NL)
             (newline-and-indent))
            ((stringp fragment)
             (insert fragment))
            ((and (consp fragment) (eq (car fragment) 'SEXPAGON))
             (sexprw-emit-sexpagon (cdr fragment)))
            (t (error "Bad output: %S" (car output)))))))

(provide 'sexprw-text)
;;; sexprw-text.el ends here.
