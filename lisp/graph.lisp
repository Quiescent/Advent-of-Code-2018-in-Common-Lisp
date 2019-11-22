(ql:quickload "iterate")

(defpackage :graph
  (:use :common-lisp)
  (:use :iter)
  (:export tuples-to-graph)
  (:export reverse-graph))

(in-package :graph)

(defun tuples-to-graph (tuples)
  (let ((graph (make-hash-table :test #'equal)))
    (dolist (tuple tuples graph)
      (when (consp (cdr tuple))
        (error "Expected tuple."))
      (let ((from (car tuple))
            (to   (cdr tuple)))
        (setf (gethash from graph)
              (delete-duplicates (push to (gethash from graph))))))))

(defun reverse-graph (graph)
  (let ((reversed (make-hash-table :test #'equal)))
    (iter
      (for (from tos) in-hashtable graph)
      (iter
        (for to in tos)
        (setf (gethash to reversed)
              (adjoin from (gethash to reversed))))
      (finally (return reversed)))))

(in-package :iter)

(defmacro-driver (FOR var DFS-ACROSS-GRAPH graph &sequence)
  "Search GRAPH depth-first, starting at the value of the FROM keyword binding VAR.

Supports the BY keyword which should be a one argument function,
accepting a list of the nodes available in the search now and should
produce the next node."
  (let ((current-nodes      (gensym))
        (g                  (gensym))
        (kwd                (if generate 'generate 'for))
        (non-default-search (not (eq 1 by))))
    (cond
      ((not (null upfrom))     (error "DFS-ACROSS-GRAPH doesn't support UPFROM"))
      ((not (null downfrom))   (error "DFS-ACROSS-GRAPH doesn't support DOWNFROM"))
      ((not (null to))         (error "DFS-ACROSS-GRAPH doesn't support TO"))
      ((not (null downto))     (error "DFS-ACROSS-GRAPH doesn't support DOWNTO"))
      ((not (null above))      (error "DFS-ACROSS-GRAPH doesn't support ABOVE"))
      ((not (null below))      (error "DFS-ACROSS-GRAPH doesn't support BELOW"))
      ((not (null with-index)) (error "DFS-ACROSS-GRAPH doesn't support WITH-INDEX"))
      ((null from)             (error "you must specify where the search should starte with FROM")))
    `(progn
       (with ,g             = ,graph)
       (with ,current-nodes = (list ,from))
       (while ,current-nodes)
       (initially (setq ,var (if ,non-default-search
                                 (funcall ,by ,current-nodes)
                                 (car ,current-nodes))))
       (,kwd ,var next (if ,non-default-search
                           (funcall ,by ,current-nodes)
                           (car ,current-nodes)))
       (setq ,current-nodes (delete ,var ,current-nodes))
       (setq ,current-nodes (append (gethash ,var ,g) ,current-nodes)))))

(defmacro-driver (FOR var DFS-ACROSS-GRAPH-WITHOUT-DUPLICATES graph &sequence)
  "Search GRAPH depth-first, starting at the value of the FROM keyword binding VAR.

Supports the BY keyword which should be a one argument function,
accepting a list of the nodes available in the search now and should
produce the next node."
  (let ((current-nodes      (gensym))
        (g                  (gensym))
        (visited            (gensym))
        (kwd                (if generate 'generate 'for))
        (non-default-search (not (eq 1 by))))
    (cond
      ((not (null upfrom))     (error "DFS-ACROSS-GRAPH doesn't support UPFROM"))
      ((not (null downfrom))   (error "DFS-ACROSS-GRAPH doesn't support DOWNFROM"))
      ((not (null to))         (error "DFS-ACROSS-GRAPH doesn't support TO"))
      ((not (null downto))     (error "DFS-ACROSS-GRAPH doesn't support DOWNTO"))
      ((not (null above))      (error "DFS-ACROSS-GRAPH doesn't support ABOVE"))
      ((not (null below))      (error "DFS-ACROSS-GRAPH doesn't support BELOW"))
      ((not (null with-index)) (error "DFS-ACROSS-GRAPH doesn't support WITH-INDEX"))
      ((null from)             (error "you must specify where the search should starte with FROM")))
    `(progn
       (with ,visited       = (make-hash-table :test #'equal))
       (with ,g             = ,graph)
       (with ,current-nodes = (list ,from))
       (while ,current-nodes)
       (initially (setq ,var (if ,non-default-search
                                 (funcall ,by ,current-nodes)
                                 (car ,current-nodes))))
       (,kwd ,var next (if ,non-default-search
                           (funcall ,by ,current-nodes)
                           (car ,current-nodes)))
       (setq ,current-nodes (delete ,var ,current-nodes))
       (when (gethash ,var ,visited)
         (next-iteration))
       (setf (gethash ,var ,visited) t)
       (setq ,current-nodes (union (gethash ,var ,g) ,current-nodes)))))



;; Scratch

;; (defvar temp-graph)
;; (setq temp-graph (graph:tuples-to-graph '((#\a . #\b) (#\a . #\c) (#\c . #\d) (#\b . #\f) (#\b . #\c))))

;; (print "Standard:")

;; (iter
;;   (for x dfs-across-graph temp-graph from #\a)
;;   (print x))

;; (print "")

;; (iter
;;   (for x dfs-across-graph temp-graph from #\a by (lambda (xs) (car (last xs))))
;;   (print x))

;; (print "No duplicates:")

;; (iter
;;   (for x dfs-across-graph-without-duplicates temp-graph from #\a)
;;   (print x))
