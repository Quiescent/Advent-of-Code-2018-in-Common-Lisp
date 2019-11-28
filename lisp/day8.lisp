;;; day8 --- My solution to day8 -*-

;;; Commentary:
;; My solution to advent of code: day8

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")

(defpackage :day8
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :iter))

(in-package :day8)

;; # PART 1:

(defun day8-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((graph (car input-elements)))
    (cdr (sum-metadata graph))))

(defun sum-metadata (numbers)
  (if (null numbers)
      (cons Nil 0)
      (progn
        (iter
         (with children     = (car  numbers))
         (with metadata     = (cadr numbers))
         (with previous-end = (cddr numbers))
         (for i from 0 below children)
         (for (end . metadata-sum) = (sum-metadata previous-end))
         (setf previous-end end)
         (summing metadata-sum into all-sums)
         (finally (return (cons (nthcdr metadata previous-end)
                                (+ all-sums
                                   (apply #'+ (subseq previous-end
                                                      0
                                                      metadata))))))))))

;; # PART 2:

(defun day8-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (cdr (sum-metadata-modified (car input-elements))))

(defun sum-metadata-modified (numbers)
  (if (null numbers)
      (cons Nil 0)
      (progn
        (iter
          (with children     = (car  numbers))
          (with metadata     = (cadr numbers))
          (with previous-end = (cddr numbers))
          (for i from 0 below children)
          (for (end . metadata-sum) = (sum-metadata-modified previous-end))
          (setf previous-end end)
          (collecting metadata-sum into metadata-sums :result-type 'vector)
          (finally (return (cons (nthcdr metadata previous-end)
                                 (if (eq children 0)
                                     (apply #'+ (subseq previous-end
                                                        0
                                                        metadata))
                                     (iter
                                       (for i in (mapcar #'1- (subseq previous-end 0 metadata)))
                                       (when (and (>= i 0)
                                                  (< i (length metadata-sums)))
                                         (sum (aref metadata-sums i))))))))))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '((2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2)))
        (expected-1 138)
        (input-2 '((2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2)))
        (expected-2 66))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day8-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day8-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-numbers "day8-part-1"))
        (input-2 (file-lines-numbers "day8-part-1")))
    (format t "
Part 1: ~s
" (day8-part-1 input-1))
    (format t "
Part 2: ~s
" (day8-part-2 input-2))))

