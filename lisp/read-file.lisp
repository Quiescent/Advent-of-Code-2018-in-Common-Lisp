(ql:quickload "str")
(ql:quickload "cl-ppcre")

(defpackage :read-file
  (:use :common-lisp)
  (:use :str))

(in-package :read-file)

(defun file-lines (file-name)
  "Read the lines from FILE-NAME into a list."
  (with-open-file (in file-name)
    (loop
      for line = (read-line in nil nil)
      while line
      collect line)))

(defun file-lines-numbers (file-name)
  "Read the lines from FILE-NAME into a list and extract all numbers.

Numbers are separated by anything other than a number."
  (mapcar (lambda (line) (mapcar #'read-from-string
                                 (words (ppcre:regex-replace-all "[^0-9]"
                                                                 line
                                                                 " "))))
          (file-lines file-name)))
