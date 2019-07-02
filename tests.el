;;; tests.el --- part of sexp-rewrite package  -*- lexical-binding:t -*-

;; Copyright 2013-2019 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later.
;; See the top of the sexp-rewrite.el file for details.

(require 'sexp-rewrite)
(defmacro sexprw--deftest (name &rest tests)
  "Auxillary macro for `sexprw-deftests'"
  ``(,',name ,(every #'identity
                   (list ,@tests))))

(defun sexprw-success/fail (results)
  (list (every #'(lambda (r) (and (car r) (cadr r))))
        results))

(defmacro sexprw-deftests (&rest tests)
  "Define a list of tests in (name &rest tests) format, and they
will be tested together and accumulated."
  (let ((result (gensym)))
     `(let ((,result (list ,@(cl-loop for x in tests
                                     collect `(sexprw--deftest ,x)))))
        (list (if (every #'identity (flatten ,result)) 'SUCCESS 'FAILURE)
             ,result))))

(defun whitetests ()
  "Tests that expose internals."
  (sexprw-deftests
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

(defun blacktests ()
  "Tests that expose 'public' (whatever that means) tests (i.e. not white).")

(provide 'sexp-rewrite-tests)
;;; tests.el ends here
