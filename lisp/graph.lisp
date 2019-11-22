(ql:quickload "iterate")

(defpackage :graph
  (:use :common-lisp)
  (:use :iter)
  (:export tuples-to-graph))

(in-package :graph)

(defun tuples-to-graph (tuples)
  (let ((graph (make-hash-table :test #'equal)))
    (dolist (tuple tuples graph)
      (when (consp (cdr tuple))
        (error "Expected tuple."))
      (let ((from (car tuple))
            (to   (cdr tuple)))
        (push to (gethash from graph))))))

(in-package :iter)

(defmacro-driver (FOR var DFS-ACROSS-GRAPH graph &sequence)
  "Search GRAPH depth-first, starting at the value of the FROM keyword binding VAR.

TODO: Supports the BY keyword which should be a one argument function,
accepting a list of the nodes available in the search now and should
produce the next node."
  (let ((current-nodes      (gensym))
        (g                  (gensym))
        (kwd                (if generate 'generate 'for))
        (non-default-search (not (eq 1 by))))
    (cond
      ((not (null upfrom))     (error "dfs-across-graph doesn't support upfrom"))
      ((not (null downfrom))   (error "dfs-across-graph doesn't support downfrom"))
      ((not (null to))         (error "dfs-across-graph doesn't support to"))
      ((not (null downto))     (error "dfs-across-graph doesn't support downto"))
      ((not (null above))      (error "dfs-across-graph doesn't support above"))
      ((not (null below))      (error "dfs-across-graph doesn't support below"))
      ((not (null with-index)) (error "dfs-across-graph doesn't support with-index")))
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



;; Scratch

;; (defvar temp-graph)
;; (setq temp-graph (graph:tuples-to-graph '((#\a . #\b) (#\a . #\c) (#\c . #\d) (#\b . #\f))))

;; (iter
;;   (for x dfs-across-graph temp-graph from #\a)
;;   (print x))

;; (print "")

;; (iter
;;   (for x dfs-across-graph temp-graph from #\a by (lambda (xs) (car (last xs))))
;;   (print x))
