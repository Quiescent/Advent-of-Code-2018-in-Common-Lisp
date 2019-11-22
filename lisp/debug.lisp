(defpackage :debug
  (:use :common-lisp)
  (:export debug-hashtable))

(defun debug-hashtable (table)
  (format nil
          "(~{~a~^
~})"
          (let ((key-values))
            (maphash (lambda (key value) (push (cons key value) key-values))
                     table)
            key-values)))
