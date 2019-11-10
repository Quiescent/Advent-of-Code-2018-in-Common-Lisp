(defun file-lines (file-name)
  "Read the lines from FILE-NAME into a list."
  (with-open-file (in file-name)
    (loop
      for line = (read-line in nil nil)
      while line
      collect line)))
