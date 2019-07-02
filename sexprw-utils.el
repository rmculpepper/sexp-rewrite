;;; utils.el --- part of sexp-rewrite package  -*- lexical-binding:t -*-

;; Copyright 2013-2019 Ryan Culpepper.
;; Released under the terms of the GPL version 3 or later.
;; See the top of the sexp-rewrite.el file for details.

(defun sexprw-utils-min-length (min list)
  "Efficiently ensure that the length of LIST is no less than MIN."
  (if (= 0 min)
      t
    (unless (null list)
        (sexprw-utils-min-length (1- min) (cdr list)))))

(defun sexprw-utils-max-length (max list)
  "Efficiently ensure that the length of LIST is no greater than MAX."
  (if (null list)
      max
      (unless (= 0 max)
        (sexprw-utils-max-length (1- max) (cdr list)))))

(defun sexprw-utils-length= (a b)
  "Efficiently ensure that the length of list A is the same as list B.
If A is an integer, then ensure that B is of length A."
  (if (integerp a)
      (cond ((and (null b) (= 0 a)) t)
            ((and (> a 0) b) (sexprw-utils-length= (1- a) (cdr b))))
      (cond ((and a b) (sexprw-utils-length= (cdr a) (cdr b)))
            ((every #'null (list a b)) t))))

(defun sexprw-utils-empty (str)
  (zerop (length str)))

(provide 'sexp-rewrite-utils)
;;; utils.el ends here
