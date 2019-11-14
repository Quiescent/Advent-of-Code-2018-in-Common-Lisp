(ql:quickload "str")
(ql:quickload "cl-ppcre")

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
  (str:words (ppcre:regex-replace "[^0-9]" (file-lines file-name) " ")))
