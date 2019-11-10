(load "read-file.lisp")

(defun day-2-part-1 ()
  "Solve day-2 part 1."
  (multiple-value-bind (twos threes) (count-2-3s (file-lines "day-2-part-1.in"))
    (* twos threes)))

(defun 2-and-3 (s)
  "Produce values (1 1) if S contains a letter 2 and/or 3 times respectively, else 0."
  (let ((counts (make-hash-table)))
    (loop
      for c across s
      do (incf (gethash c counts 0)))
    (loop
      for key being the hash-keys in counts
      with has-three = 0
      with has-two   = 0
      for count = (gethash key counts)
      when (eq count 2)
        do (setf has-two 1)
      when (eq count 3)
        do (setf has-three 1)
      finally (return (values has-three has-two)))))

(defun count-2-3s (codes)
  "Count the number of two's and three's."
  (loop
    for code in codes
    with twos   = 0
    with threes = 0
    do (multiple-value-bind (two three) (2-and-3 code)
         (progn (incf twos two)
                (incf threes three)))
    finally (return (values twos threes))))

(defun day-2-part-2 ()
  "Solve day-2 part 2."
  (multiple-value-bind (s1 s2) (differ-by-one (file-lines "day-2-part-1.in"))
    (eliminate-difference s1 s2)))

(defun differ-by-one (codes)
  "Produce the two CODES which differ by one position."
  (loop
    named outer
    for (current . rest) on codes
    do (loop
         for other-code in rest
         when (eq 1 (distance current other-code))
           do (return-from outer (values current other-code)))))

(defun distance (s1 s2)
  "Produce the number of characters positions which differ between S1 and S2"
  (reduce #'+ (map 'list (lambda (c1 c2) (if (eq c1 c2) 0 1)) s1 s2)))

(defun eliminate-difference (s1 s2)
  "Produce a string from S1 and S2 which has non-common positions removed."
  (coerce (remove-if #'null (map 'list (lambda (c1 c2) (if (eq c1 c2) c1 nil)) s1 s2)) 'string))
