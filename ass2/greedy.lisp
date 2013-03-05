(load "search-aux.lisp")
(defun h-hat (node) (fourth node))

(defun greedy-succ (node ops sg)
  (let ((s-nodes nil) (s-states nil) (n-state (state node)))
    (loop for op in ops
       do
	 (setf s-states `(,(funcall op n-state))) ;new states created
	 (setf s-nodes 
	       (append s-nodes
		      (mapcar (lambda (s-state) ;new states --> nodes
				(cond
				((null s-state) nil)
				(t `(,s-state 
				     ,op 
				     ,node 					      
				     ,(manhat s-state sg)))))
			      s-states))))
    s-nodes))

(defun greedy (s0 sg stats?)
  (let ((ahead `((,s0 ,nil ,nil ,(manhat s0 sg)))) ;list of unexplored nodes, starting with s0
	(behind nil) ;list of explored nodes
	(n nil)
	(children nil)
	(node-count 1)
	(redundancy 0))
    (loop
       (if (null ahead) (return-from greedy "no goal state found :("))
       (setf n (pop ahead)) ;set N to first explored node
       (if (equal (state n) sg) ;if goal state has been reached
	   (if stats?
	       (return-from greedy (cons (remove nil (solution n))
				      `(,node-count 
					,redundancy
					,(length ahead)
					,(length behind))))
	       (return-from greedy (remove nil (solution n)))))
       (push n behind)
					;collect n's child nodes into a list
       (setf children (remove nil (greedy-succ n `(,#'north ,#'south ,#'east ,#'west) sg)))
       (setf node-count (+ node-count (length children)))
       (let ((temp (length children)))
;get rid of nodes with states we already have covered
	 (setf children (diff children behind))
	 (setf redundancy (+ redundancy (- temp (length children)))))
       (setf ahead (append ahead children))
       (setf ahead
	     (sort ahead (lambda (n1 n2) (< (h-hat n1) (h-hat n2))))))))