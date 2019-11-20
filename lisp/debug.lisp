(defun debug-hashtable (table)
  (format nil
          "(~{~$
 ~a~})"
          (let ((key-values))
            (maphash (lambda (key value) (push (cons key value) key-values))
                     table)
            key-values)))
