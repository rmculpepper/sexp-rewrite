;;; sexprw-tests.el --- Part of sexp-rewrite package. -*- lexical-binding:t -*-

;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;; Copyright 2013-2019 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later.
;; See the top of the sexp-rewrite.el file for details.
;; URL: https://github.com/rmculpepper/sexp-rewrite
;; Homepage: https://github.com/rmculpepper/sexp-rewrite

;;; Commentary:
;; Tests for sexprw and sexprw-utils.

;;; Code:

(require 'sexp-rewrite)
(defmacro sexprw-tests--deftest (name &rest tests)
  "Auxillary macro for `sexprw-tests-deftests'"
  ``(,',name ,(every #'identity
                   (list ,@tests))))

(defun sexprw-tests-success-fail (&rest results)
  (list (if (every #'(lambda (r) (and (car r) (cadr r)))
                   results)
            'success
          'failure)
        results))

(defmacro sexprw-tests-deftests (&rest tests)
  "Define a list of tests in (name &rest tests) format, and they
will be tested together and accumulated."
  `(sexprw-tests-success-fail
    ,@(cl-loop for x in tests
              collect (cons 'sexprw-tests--deftest x))))

(defun sexprw-tests-whitetest ()
  "Tests that expose internals."
  (sexprw-tests-deftests
   (sexprw-max-length (sexprw-max-length 0 nil)
                      (sexprw-max-length 1 nil)
                      (sexprw-max-length 1 '(1)))
   (sexprw-min-length (sexprw-min-length 0 nil)
                      (not (sexprw-min-length 1 nil))
                      (sexprw-min-length 1 '(1)))
   (sexprw-length= (sexprw-length= '(1) '(2))
                   (null (sexprw-length= nil '(2)))
                   (sexprw-length= 3 '(1 2 3))
                   (null (sexprw-length= 1 '(1 2))))
   (sexprw-empty (sexprw-empty "")
                 (null (sexprw-empty "asdf")))))

(defun sexprw-tests-black ()
  "Tests that expose 'public' (whatever that means) tests (i.e. not white).")

(provide 'sexprw-tests)
;;; sexprw-tests.el ends here
