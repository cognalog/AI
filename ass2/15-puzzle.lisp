(defun 15-puzzle (s0 sg st)
  (print 'BFS)
  (print (bfs s0 sg st))
  (print 'DFS) 
  (print 'depth| limit: 8|) (print (dfs s0 sg 8 st))
  (print 'depth| limit: 10|) (print (dfs s0 sg 10 st))
  (print 'IDDFS) (print (iddfs s0 sg 8 3 st))
  (print 'UCS) (print (ucs s0 sg st))
  (print 'GREEDY)
  (print 'mismatch| heuristic|) (print (greedy s0 sg #'mis-match st))
  (print 'manhattan| heuristic|) (print (greedy s0 sg #'manhat st))
  (print 'A*)
  (print 'mismatch| heuristic|) (print (a-star s0 sg #'mis-match st))
  (print 'manhattan| heuristic|) (print (a-star s0 sg #'manhat st))
  t)

(defun hardcore (s0 sg st)
  (print 'GREEDY)
  (print 'manhattan| heuristic|) (print (greedy s0 sg #'manhat st))
  (print 'A*)
  (print 'mismatch| heuristic|) (print (a-star s0 sg #'mis-match st))
  (print 'manhattan| heuristic|) (print (a-star s0 sg #'manhat st))
  t)

(defun main (st)
  (load "search-aux.lisp")
  (load "bfs.lisp")
  (load "dfs.lisp")
  (load "ucs.lisp")
  (load "greedy.lisp")
  (load "a-star.lisp")
  (format t "~%~%~a~%" 'GOAL| STATE 1|)
  (format t "~%~%~a~%" 'EASY)
  (print (15-puzzle '((1 2 3 0) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 3))
		    '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0))
		    st))
  (format t "~%~%~a~%" 'MODERATE)
  (print (15-puzzle '((1 2 6 3) (4 5 10 7) (0 9 14 11) (8 12 13 15) (2 0))
		    '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0))
		    st))
  (format t "~%~%~a~%" 'HARD)
  (print (hardcore '((1 2 3 7) (4 5 6 15) (8 9 11 0) (12 13 14 10) (2 3))
		    '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0))		    
		    st))
  (format t "~%~%~a~%" 'GOAL| STATE 2|)
  (format t "~%~%~a~%" 'EASY)
  (print (15-puzzle '((1 2 3 4) (8 7 6 5) (9 10 11 12) (15 14 13 0) (3 3)) 
		    '( (1 2 3 4) (8 7 6 5) (9 10 11 12) (0 15 14 13) (3 0)) ;goal state 2
		    st))
  (format t "~%~%~a~%" 'MODERATE)
  (print (15-puzzle '((1 2 3 4) (8 10 7 5) (9 0 6 11) (15 14 13 12) (2 1))
		    '( (1 2 3 4) (8 7 6 5) (9 10 11 12) (0 15 14 13) (3 0))
		    st))
  (format t "~%~%~a~%" 'HARD)
  (print (hardcore '((2 3 4 0) (1 11 7 6) (8 9 14 5) (15 10 13 12) (0 3))
		   '( (1 2 3 4) (8 7 6 5) (9 10 11 12) (0 15 14 13) (3 0))
		    st))
)

;bfs with hard problem
(bfs '((1 2 6 3) (4 5 10 7) (0 9 14 11) (8 12 13 15) (2 0))
     '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0)) 
     t)
;dfs with hard problem
(dfs '((1 2 6 3) (4 5 10 7) (0 9 14 11) (8 12 13 15) (2 0))
     '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0))
     17
     t)
;iddfs with hard problem
(iddfs '((1 2 6 3) (4 5 10 7) (0 9 14 11) (8 12 13 15) (2 0))
       '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0))
       8 3
       t)
;ucs with hard problem
(ucs '((1 2 6 3) (4 5 10 7) (0 9 14 11) (8 12 13 15) (2 0))
     '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15) (0 0)) 
     t)