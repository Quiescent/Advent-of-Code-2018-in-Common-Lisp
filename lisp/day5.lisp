;;; day5 --- My solution to day5 -*-

;;; Commentary:
;; My solution to advent of code: day5

;;; Code:

(ql:quickload "iterate")

(defpackage :day5
  (:use :common-lisp)
  (:use :iter))

(in-package :day5)

;; # PART 1:

(load "read-file.lisp")

(defun day5-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((polymer-as-list (map 'list #'identity input-elements)))
    (length (remove-all-reacting polymer-as-list))))

(defun polymers-react (a b)
  "Produce t if A and B react."
  (and (not (eq a b))
       (eq (cl-unicode:lowercase-mapping a)
           (cl-unicode:lowercase-mapping b))))

(defun remove-reacting-one-pass (polymer)
  (iter
    (with pointer  = polymer)
    (with changed  = nil)
    (with previous = nil)
    (while pointer)
    (for a = (car  pointer))
    (for b = (cadr pointer))
    (when (and (consp (cdr pointer))
               (polymers-react a b))
      (setf changed t)
      (if (null (caddr pointer))
          (setf (cdr previous) nil)
          (setf (car pointer) (caddr pointer)
                (cdr pointer) (cdddr pointer))))
    (setf previous pointer)
    (pop pointer)
    (finally (return changed))))

(defun remove-all-reacting (polymer)
  "Remove reacting bases from the list POLYMER."
  (iter
    (while (remove-reacting-one-pass polymer))
    (finally (return polymer))))

;; # PART 2:

(defun day5-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (length (find-best-removal input-elements)))

(defun char-equals-ignore-case (a b)
  (eq (cl-unicode:lowercase-mapping a)
      (cl-unicode:lowercase-mapping b)))

(defun find-best-removal (polymer)
  (iter
    (for x in-string "abcdefghijklmnopqrstuvwxyz")
    (finding x minimizing (length (remove-all-reacting
                                   (remove-if (lambda (y) (char-equals-ignore-case y x))
                                              (map 'list #'identity polymer))))
             into best)
    (finally (return (remove-all-reacting
                      (remove-if (lambda (y) (char-equals-ignore-case y best))
                                 (map 'list #'identity polymer)))))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '())
        (expected-1 '())
        (input-2 '())
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day5-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day5-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (car (file-lines "day5-part-1")))
        (input-2 (car (file-lines "day5-part-1"))))
    (format t "
Part 1: ~s
" (day5-part-1 input-1))
    (format t "
Part 2: ~s
" (day5-part-2 input-2))))

