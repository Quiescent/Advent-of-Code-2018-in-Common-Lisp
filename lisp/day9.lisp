;;; day9 --- My solution to day9 -*-

;;; Commentary:
;; My solution to advent of code: day9

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day9
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day9)

;; # PART 1:

(defun day9-part-1 (player-count marble-count)
 "Run my solution to part one of the problem with PLAYER-COUNT players for  MARBLE-COUNT marbles."
  (let ((players (make-array (list player-count) :initial-element 0))
        (marbles (make-array (list marble-count) :initial-element nil))
        (len     1)
        (idx     0))
    (setf (aref marbles 0) 0)
    (iter
      (for i from 1 to marble-count)
      (when (eq 0 (mod i 23))
        (for current-player = (mod i (length players)))
        (incf (aref players current-player) i)
        (setf idx (mod (- idx 7) len))
        (incf (aref players current-player) (aref marbles idx))
        (remove-at idx marbles)
        (decf len)
        (next-iteration))
      (setf idx (insert i idx len marbles))
      (incf len))
    (reduce #'max players)))

(defun insert (val idx len marbles)
  (let ((inserted-at (mod (+ idx 2) len)))
    (insert-at (mod (+ idx 2) len) val marbles)
    inserted-at))

(defun remove-at (idx marbles)
  (iter
    (for i from idx below (1- (length marbles)))
    (setf (aref marbles i) (aref marbles (1+ i)))))

(defun insert-at (idx val marbles)
  (progn
    (iter
      (for i from (1- (length marbles)) downto (1+ idx))
      (setf (aref marbles i) (aref marbles (1- i))))
    (setf (aref marbles idx) val)))

;; (let ((marbles (map 'vector #'identity #(1 2 3 nil nil nil))))
;;   (remove-at 0 marbles)
;;   (format t "~%~a~%" marbles))

;; # PART 2:

(defun day9-part-2 (player-count marble-count)
  "Run my solution to part one of the problem with PLAYER-COUNT players for  MARBLE-COUNT marbles."
  (let* ((players (make-array (list player-count) :initial-element 0))
         (marbles (list (cons 0 nil)))
         (ptr marbles)
         (*print-circle* t))
    (setf (cdar marbles) marbles)
    (setf (cdr marbles)  marbles)
    (iter
      (for i from 1 to marble-count)
      (when (eq 0 (mod i 23))
        (for current-player = (mod i (length players)))
        (incf (aref players current-player) i)
        (iter
          (for i from 0 below 8)
          (setf ptr (cdar ptr)))
        (incf (aref players current-player) (caadr ptr))
        (setf (cdr ptr) (cddr ptr))
        (setf (cdaddr ptr) ptr)
        (setf ptr (cdr ptr))
        (next-iteration))
      (setf ptr (cdr ptr))
      (for new-cell = (cons (cons i ptr) (cdr ptr)))
      (setf (cdadr ptr) new-cell)
      (setf (cdr ptr)   new-cell)
      (setf ptr new-cell))
    (reduce #'max players)))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (format t "
Part 1:
Expected: ~s
     Got: ~s
" 32 (day9-part-2 9 25))
  ;; (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day9-part-2 input-2))
  )

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (format t "
Part 1: ~s
" (day9-part-2 426 72058))
  (format t "
Part 2: ~s
" (day9-part-2 426 7205800)))

