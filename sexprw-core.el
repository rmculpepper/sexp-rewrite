;;; sexprw-core.el --- core pattern functions  -*- lexical-binding:t -*-

;; Copyright 2013-2019 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later;
;; see sexprw.el for details.

(require 'sexprw-text)

;; ============================================================
;; NT Definitions

;; Nonterminal (nt) names have property 'sexprw-nt with value
;; (list 'nt P attrs docstring), where attrs is list of symbol.

(defun sexprw-nt-symbolp (sym)
  (and (get sym 'sexprw-nt) t))

(defun sexprw-nt-value (sym)
  (or (and (symbolp sym) (get sym 'sexprw-nt))
      (error "Not a sexp-rewrite nt name: %S" sym)))


;; ============================================================
;; Debugging and diagnostics

(defvar sexprw-current-operation nil
  "Name of currently executing operation.")

(defvar sexprw-failure-info nil
  "Information about last sexp-rewrite failure(s).")

(defun sexprw-fail (info)
  (push (cons sexprw-current-operation (cons (point) info)) sexprw-failure-info)
  nil)


;; ============================================================
;; Core patterns

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
  "Match the sexp starting at point against core PATTERN,
returning an \(list ENV) mapping the pattern variables of
PATTERN to fragments, or nil on failure.  Advances point to end
of matched term(s).

The following grammar describes core patterns:

  P ::= (LIST P*)
      | (SPLICE P*)
      | (quote symbol)
      | (VAR symbol nt)
      | (pREP P Pk)
      | (AND P*)
      | (OR P*)
      | (NOT P)
      | (GUARD P expr)

Matching builds an Env: an alist mapping pvar symbols to EnvValue.

  EnvValue ::= Block                    ; see `sexprw-range-to-block'
             | (cons 'rep EnvValue)     ; representing depth>0 list
             | (cons 'pre PreOutput)    ; representing computed output

The (pREP P Pk) pattern represents (P ... Pk): match as many P as
possible such that it is still possible to match Pk afterwards (then
commit). Handling together avoids (non-local) backtracking while 
supporting non-trivial Pks."
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
        ((eq (car pattern) 'NOT)
         (let* ((init-point (point))
                (result (let ((sexprw-failure-info nil)) ;; fluid-let
                          (sexprw-match (cadr pattern)))))
           (cond (result
                  (goto-char init-point)
                  (sexprw-fail `(match not env= ,(car result))))
                 (t (goto-char (point-max)) (list nil)))))
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
                                 (sexprw-looking-at-whitespace-to-eof)))
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

(defun sexprw-check-guard-result (result _env)
  ;; FIXME: check result is nil or (list extension-of-env)?
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
                             (or (sexprw-looking-at-whitespace-to-eof)
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

(defun sexprw-looking-at-whitespace-to-eof ()
  "Return t if the rest of the (restricted) buffer contains only whitespace."
  (looking-at "[[:space:]\n]*\\'"))


;; ============================================================
;; Match environment functions

(defun sexprw-env-ref (env key)
  "Fetch the value associated with KEY in ENV, or nil otherwise."
  (let ((result (assq key env)))
    (and result (cdr result))))

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
         ;; FIXME: could compare sexpagons (if exist), slightly more equalities
         (equal (sexprw-block-text a)
                (sexprw-block-text b)))
        (t nil)))


;; ============================================================
;; User-level Guard utilities

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

(defun sexprw-guard-all-same-atom (env pvar)
  (let ((repval (sexprw-env-ref env pvar)))
    (and (consp repval) (eq (car repval) 'rep) (consp (cdr repval))
         (let ((the-val (cadr repval))
               (ok t))
           (dolist (val (cdr repval))
             (unless (sexprw-entry-equal val the-val) (setq ok nil)))
           (if ok the-val nil)))))

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

(defvar sexprw-template*-multiline nil ;; boolean
  "True when (hard) NL or multi-line block occurs in current LIST/SQLIST.")

(defun sexprw-template* (template env) ;; PreOutput
  "Interpret core TEMPLATE using the pattern variables of ENV
to produce a PreOutput.

The following grammar describes core templates:

  T ::= string          ; literal text, eg \n inserts non-latent newline
      | (quote symbol)  ; literal symbol
      | (VAR symbol)    ; pattern variable
      | (LIST T*)       ; parenthesized list
      | (SQLIST T*)     ; bracketed list
      | (SPLICE T*)     ; spliced list contents
      | (SP)            ; latent space (ie, change latent newline to latent space)
      | (SL)            ; latent soft newline: if surrounding list has any 
                        ; NLs or multi-line blocks, NL, else ignore
      | (NL)            ; latent newline
      | (tREP T vars)   ; repetition

The result is PreOutput:

  PreOutput = (treeof PreOutputPart)
  PreOutputPart ::= string
                  | SP
                  | NL
                  | SL
                  | NONE
                  | (SEXPAGON . ListOfString)
                  | (SL=nil . PreOutput)
                  | (SL=NL . PreOutput)

PreOutput is interpreted left to right: the *last* spacing symbol to occur wins."
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
        ((memq (car template) '(SP NL SL NONE))
         (car template))
        ((eq (car template) 'tREP)
         (sexprw-template-rep template env))))

(defun sexprw-template-rep (template env)
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
          (signal 'sexprw-template-error 'ellipsis-count-mismatch)))
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
                   (lines (sexprw-block-sexpagon value))
                   (space (if (sexprw-block-onelinep value) 'SP 'NL)))
               (unless (sexprw-block-onelinep value)
                 (setq sexprw-template*-multiline t))
               (cond ((zerop (length text))
                      ;; no space after empty block
                      nil)
                     (lines
                      (list (cons 'SEXPAGON lines) space))
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
  (let ((accum (list '()))) ; nil or (list PreOutput) -- FIXME ?!
    (dolist (inner inners)
      (setq accum (cons accum (sexprw-template* inner env))))
    accum))

;; sexprw-output*-SL : (U nil 'NL), fluid -- FIXME: remove?
(defvar sexprw-output*-SL nil)

(defun sexprw-output (pre)
  "Process the PreOutput PRE into Output.

See `sexprw-template*' for PreOutput; see `sexprw-emit' for Output."
  (let* ((result (sexprw-output* pre nil 'NONE))
         (raccum (car result))
         (_latent (cdr result)))
    (let ((sexprw-output*-SL nil)) ;; fluid-let
      (reverse raccum))))

(defun sexprw-output* (pre raccum latent)
  (cond ((and (consp pre) (eq (car pre) 'SEXPAGON))
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

;; ============================================================

(provide 'sexprw-core)
;;; sexprw-core.el ends here.
