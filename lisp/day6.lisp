;;; day6 --- My solution to day6 -*-

;;; Commentary:
;; My solution to advent of code: day6

;;; Code:

(ql:quickload "iterate")

(defpackage :day6
  (:use :common-lisp)
  (:use :iter))

(in-package :day6)

;; # PART 1:

(load "read-file.lisp")

(defun day6-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (multiple-value-bind (max-x max-y min-x min-y) (bounds-of-coords input-elements)
    (let* ((grid                (colour-in-map input-elements max-x max-y min-x min-y))
           (all-the-labels      (all-labels input-elements))
           (the-infinite-labels (infinite-labels grid max-x max-y min-x min-y))
           (non-infinite-labels (set-difference all-the-labels the-infinite-labels))
           (counts              (count-areas grid)))
      (largest-bound counts non-infinite-labels))))

(defun colour-in-map (coordinates max-x max-y min-x min-y)
  (iter
    (for x from (1- min-x) to (1+ max-x))
    (with grid = (make-array (list (+ 2 max-x) (+ 2 max-y)) :initial-element nil))
    (iter
      (for y from (1- min-y) to (1+ max-y))
      (setf (aref grid x y) (closest-points-label x y coordinates)))
    (finally (return grid))))

(defun closest-points-label (x y coordinates)
  (iter
    (for (x2 y2) in coordinates)
    (with distances)
    (for i from 0)
    (for current-distance = (+ (abs (- x x2))
                               (abs (- y y2))))
    (push current-distance distances)
    (finding (cons i current-distance) minimizing current-distance into closest-label)
    (finally (return (when (eq 1 (count (cdr closest-label) distances))
                       (car closest-label))))))

(defun all-labels (coordinates)
  (iter
    (for x in coordinates)
    (for i from 0)
    (collect i)))

(defun largest-bound (counts non-infinite-labels)
  (iter
    (for label in non-infinite-labels)
    (finding label maximizing (gethash label counts) into best-label)
    (finally (return (gethash best-label counts)))))

(defun count-areas (grid)
  (iter
    (with counts = (make-hash-table))
    (for x from 0 below (array-dimension grid 0))
    (iter
      (for y from 0 below (array-dimension grid 1))
      (for label = (aref grid x y))
      (when (null (gethash label counts))
        (setf (gethash label counts) 0))
      (incf (gethash label counts)))
    (finally (return counts))))

(defun infinite-labels (grid max-x max-y min-x min-y)
  (union
   (iter
     (for x from (1- min-x) to (1+ max-x))
     (adjoining (aref grid x max-y))
     (adjoining (aref grid x min-y)))
   (iter
     (for y from (1- min-y) to (1+ max-y))
     (adjoining (aref grid max-x y))
     (adjoining (aref grid min-x y)))))

(defun bounds-of-coords (coordinates)
  (iter
    (for (x y) in coordinates)
    (maximizing x into max-x)
    (maximizing y into max-y)
    (minimizing x into min-x)
    (minimizing y into min-y)
    (finally (return (values max-x max-y min-x min-y)))))

;; # PART 2:

(defun day6-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (multiple-value-bind (max-x max-y min-x min-y) (bounds-of-coords input-elements)
    (let ((diff-x (- 10000 (- max-x min-x)))
          (diff-y (- 10000 (- max-y min-y))))
      (iter outer
        (for x from (- min-x diff-x) to (+ max-x diff-x))
        (iter
          (for y from (- min-y diff-y) to (+ max-y diff-y))
          (in outer
              (counting (within-10000 x y input-elements))))))))

(defun within-10000 (x y coordinates)
  (< (apply #'+ (mapcar (lambda (xy) (+ (abs (- x (car  xy)))
                                        (abs (- y (cadr xy)))))
                        coordinates))
     10000))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '((1 1)
                   (1 6)
                   (8 3)
                   (3 4)
                   (5 5)
                   (8 9)))
        (expected-1 17))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day6-part-1 input-1))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-numbers "day6-part-1"))
        (input-2 (file-lines-numbers "day6-part-1")))
    (format t "
Part 1: ~s
" (day6-part-1 input-1))
    (format t "
Part 2: ~s
" (day6-part-2 input-2))))

