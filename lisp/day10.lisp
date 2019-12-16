;;; day10 --- My solution to day10 -*-

;;; Commentary:
;; My solution to advent of code: day10

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day10
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day10)

;; # PART 1:

(defun day10-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((stars (mapcar #'parse-line input-elements))
        previous-stars)
    (iter
      (for i from 1 below 20000)
      (setf previous-stars stars)
      (setf stars (advance-stars stars))
      (multiple-value-bind (p-min-x p-max-x p-min-y p-max-y) (find-bounds previous-stars)
        (multiple-value-bind (min-x max-x min-y max-y) (find-bounds stars)
          (when (and (> (- max-x min-x) (- p-max-x p-min-x))
                     (< (- p-max-x p-min-x) 120)
                     (> (- max-y min-y) (- p-max-y p-min-y))
                     (< (- p-max-y p-min-y) 80))
            (format t "~%")
            (print-stars previous-stars)
            (return)))))))

(defun advance-stars (stars)
  (iter
    (for (x y vx vy) in stars)
    (collect (list (+ x vx) (+ y vy) vx vy))))

(defun find-bounds (stars)
  (iter
    (for (x y . _) in stars)
    (minimizing x into min-x)
    (maximizing x into max-x)
    (minimizing y into min-y)
    (maximizing y into max-y)
    (finally (return (values min-x max-x min-y max-y)))))

(defun print-stars (stars)
  (let ((indexed-stars (make-hash-table :test #'equal)))
    (iter
      (for (x y . _) in stars)
      (setf (gethash (cons x y) indexed-stars) t))
    (multiple-value-bind (min-x max-x min-y max-y) (find-bounds stars)
      (iter
        (for y from min-y to max-y)
        (with star-count = 0)
        (iter
          (for x from min-x to max-x)
          (if (gethash (cons x y) indexed-stars)
              (progn
                (format t "*")
                (incf star-count))
              (format t " ")))
        (format t "~%")
        (finally (return star-count))))))

(defun parse-line (line)
  (mapcar #'read-from-string
          (remove ""
                  (ppcre:split " " (ppcre:regex-replace-all "[^-0-9]" line " "))
                  :test #'equal)))

;; # PART 2:

(defun day10-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((stars (mapcar #'parse-line input-elements))
        previous-stars)
    (iter
      (for i from 0 below 20000)
      (setf previous-stars stars)
      (setf stars (advance-stars stars))
      (multiple-value-bind (p-min-x p-max-x p-min-y p-max-y) (find-bounds previous-stars)
        (multiple-value-bind (min-x max-x min-y max-y) (find-bounds stars)
          (when (and (> (- max-x min-x) (- p-max-x p-min-x))
                     (< (- p-max-x p-min-x) 120)
                     (> (- max-y min-y) (- p-max-y p-min-y))
                     (< (- p-max-y p-min-y) 80))
            (format t "~%")
            (print-stars previous-stars)
            (return i)))))))

;; Scratch area:

;; (progn
;;   (print "********** SCRATCH **********
;; ")
;;   (let ((input-1 '())
;;         (expected-1 '())
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "
;; Part 1:
;; Expected: ~s
;;      Got: ~s
;; " expected-1 (day10-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day10-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day10-part-1"))
        (input-2 (file-lines "day10-part-1")))
    (format t "
Part 1: ~s
" (day10-part-1 input-1))
    (format t "
Part 2: ~s
" (day10-part-2 input-2))))

