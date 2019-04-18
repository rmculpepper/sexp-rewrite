;;; sexprw-text.el --- text and buffer functions  -*- lexical-binding:t -*-

;; Copyright 2013-2019 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later;
;; see sexprw.el for details.

;; ============================================================
;; Blocks

;; Block = (list 'block TEXT ONELINEP STARTCOL IMPUREPREFIX START END)

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
  "Return a Block describing the next sexp, or nil on failure.
On success, advance point to the end of the sexp.

If REQUIRE-PURE is non-nil, then there must be no non-whitespace
characters before the start of the sexp, or else nil is returned.

See `sexprw-range-to-block' for the definition of Block."
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
  "Returns a Block from the region START to END.

If PURE-START is an integer, it must satisfy START <= PURE-START < END,
and the Block represents a single sexp with a (possibly empty) \"impure
prefix\". The impure prefix consists of comments and possibly prefixed
reader abbreviations for quote, etc. Specifically, PURE-START is the
result of `backward-sexp' starting at END. If PURE-START is nil, then 
the region may represent multiple sexps or something else entirely.

  Block = (list 'block TEXT ONELINEP STARTCOL IMPUREPREFIX START END)

TEXT is a string containing the contents of the block. ONELINEP
is true if the block consists of a single line. STARTCOL is the
column where the first line of TEXT started. IMPUREPREFIX is the
length of the \"impure prefix\" if PURE-START is an integer, or
nil if PURE-START is nil."
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

(defun sexprw-sexpagon (text start-col)
  "If TEXT is a sexpagon, remove its indentation; else return nil.

A sexpagon is the shape of a well-formatted sexp:

    (----------+
    |          |
    |      +---+
    +------)

There must be no non-whitespace characters to the left of the open
paren's column from the second line to the last. Well-formatted
Lisp/Scheme/Racket code is nearly always sexpagonal, with the
occasional exception of multi-line string literals.

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

(defun sexprw-emit (parts)
  "Write the Emittable PARTS at point.

The following grammar describes Emittable:

  Emittable = (listof EmittablePart)
  EmittablePart ::= string
                  | NL
                  | (SEXPAGON . ListOfString)

A NL part means `newline-and-indent'; a SEXPAGON part means
`sexprw-emit-sexpagon'."
  (dolist (fragment parts)
    (cond ((eq fragment 'NL)
           (newline-and-indent))
          ((stringp fragment)
           (insert fragment))
          ((and (consp fragment) (eq (car fragment) 'SEXPAGON))
           (sexprw-emit-sexpagon (cdr fragment)))
          (t (error "Bad emittable part: %S" fragment)))))

(defun sexprw--emit-to-string (parts)
  "Return a string of the Emittable PARTS. No indentation is
performed on newlines, so the result is not very useful. It is
used to implement a rough form of equality for
`sexprw-output-equal'."
  (mapconcat (lambda (fragment)
               (cond ((eq fragment 'NL)
                      "\n")
                     ((stringp fragment)
                      fragment)
                     ((and (consp fragment) (eq (car fragment) 'SEXPAGON))
                      (mapconcat #'identity (cdr fragment) "\n"))
                     (t (error "Bad emittable part: %S" fragment))))
             parts ""))

(provide 'sexprw-text)
;;; sexprw-text.el ends here.
