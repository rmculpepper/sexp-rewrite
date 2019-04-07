;;; sexprw-desugar.el --- desugar surface patterns to core  -*- lexical-binding:t -*-

;; Copyright 2013-2019 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later;
;; see sexprw.el for details.

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
;;      | [ PT* ]        ~ (SQLIST T*)
;;      | (!SQ PT*)      ~ (SQLIST T*)
;;      | !NL            ~ (NL)
;;      | !SP            ~ (SP)
;;      | !SL            ~ (SL)
;;      | !NOSP          ~ (NONE)
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
          ((and template (eq pretty '!NOSP))
           '(NONE))
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

(provide 'sexprw-desugar)
;;; sexprw-desugar.el ends here.
