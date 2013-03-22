(load "search-aux.lisp")

(defun bfs (s0 sg stats?)
  (let ((ahead `((,s0 ,nil ,nil))) ;list of unexplored nodes, starting with s0
	(behind nil) ;list of explored nodes
	(n nil)
	(children nil)
	(node-count 1)
	(redundancy 0))
    (loop
       (if (null ahead) (return-from bfs "no goal state found :("))
       (setf n (pop ahead)) ;set N to first explored node
       (push n behind)
       (if (equal (state n) sg) ;if goal state has been reached
	   (if stats?
	       (return-from bfs (cons (remove nil (solution n))
				      `(,node-count 
					,redundancy
					,(length ahead)
					,(length behind))))
	       (return-from bfs (remove nil (solution n)))))
					;collect n's child nodes into a list
       (setf children (remove nil (succ-fxn n `(,#'north ,#'south ,#'east ,#'west))))
       (setf node-count (+ node-count (length children)))
       (let ((temp (length children)))
	 (setf children ;get rid of nodes with states we already have/will cover
	       (diff children (append ahead behind)))
	 (setf redundancy (+ redundancy (- temp (length children)))))
       (setf ahead (append ahead children))))) ;add newly generated nodes to frontier