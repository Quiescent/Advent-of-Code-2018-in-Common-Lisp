(load "read-file.lisp")

(defun part-1 ()
  "Solve part 1."
  (reduce #'+ (mapcar #'read-from-string (file-lines "day1-part1.in"))))

(defun part-2 ()
  (let ((numbers (mapcar #'read-from-string (file-lines "day1-part1.in"))))
    (setf (cdr (last numbers)) numbers)
    (loop
      for frequency in numbers
      with seen       = '()
      with cumulative = 0
      when (member cumulative seen)
        return cumulative
      do (push cumulative seen)
         (incf cumulative frequency))))
