(load "bfs.lisp")
(defun 15-puzzle (s0 sg)
  (load "bfs.lisp")
  (bfs s0 sg 'succ-fxn))