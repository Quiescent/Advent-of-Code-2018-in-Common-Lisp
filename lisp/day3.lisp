;;; day3 --- My solution to day3 -*-

;;; Commentary:
;; My solution to advent of code: day3

;;; Code:

;; # PART 1:

(ql:quickload "str")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")

(defun day3-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((claims-sans-id (mapcar (lambda (line) (cdr (parse-line line))) input-elements))
        (claim-map      (make-hash-table :test #'equal)))
    (dolist (claim claims-sans-id)
      (claim-fabric claim claim-map))
    (count-2-or-more claim-map)))

(defun parse-line (line)
  (mapcar #'read-from-string
          (str:words (ppcre:regex-replace-all "[^0-9]" line " "))))

(defun count-2-or-more (claims)
  (loop
    for value being the hash-values in claims
    counting (>= value 2) into squares
    finally (return squares)))

(defun claim-fabric (claim claims)
  (destructuring-bind (x y width height) claim
    (loop
      for i from x below (+ x width)
      do (loop
           for j from y below (+ y height)
           do (incf (gethash (cons i j) claims 0))))))

;; # PART 2:

(defun day3-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((claims (mapcar (lambda (line) (parse-line line)) input-elements)))
    (loop
      for claim in claims
      do (loop
           named check-others
           for other in claims
           when (and (not (eq claim other)) (intersects claim other))
             do (return-from check-others)
           finally (return (car claim))))))

(defun intersects (one other)
  (destructuring-bind (_ x1 y1 width1 height1) one
    (destructuring-bind (_ x2 y2 width2 height2) other
      (not (or (< (+ x1 width1) x2)
               (< (+ x2 width2) x1)
               (< (+ y1 height1) y2)
               (< (+ y2 height2) y1))))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"))
        (expected-1 4)
        (input-2 '("#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"))
        (expected-2 3))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day3-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day3-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day3-part-1"))
        (input-2 (file-lines "day3-part-1")))
    (format t "
Part 1: ~s
" (day3-part-1 input-1))
    (format t "
Part 2: ~s
" (day3-part-2 input-2))))

