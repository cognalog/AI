(load "search-aux.lisp")

(defun dfs-succ (node ops)
  (let ((s-nodes nil) (s-states nil) (n-state (state node)) (d (depth node)))
    (loop for op in ops
       do
	 (setf s-states `(,(funcall op n-state))) ;new states created
	 (setf s-nodes 
	       (append s-nodes
		      (mapcar (lambda (s-state) ;new states --> nodes
				(cond
				  ((null s-state) nil)
					;prevent 2-step cycles
				  (t `(,s-state ,op ,node ,(+ d 1)))))
			      s-states))))
    s-nodes))

(defun dfs (s0 sg limit stats?)
  (let ((ahead `((,s0 ,nil ,nil 0))) ;list of unexplored nodes, starting with s0
	(behind nil) ;list of explored nodes
	(n nil)
	(children nil)
	(node-count 1)
	(redundancy 0))
    (loop
       (if (null ahead) (return-from dfs "no goal state found :("))
       (setf n (pop ahead)) ;set N to first explored node
       (if (<= (depth n) limit)
	   (progn
	     (if (equal (state n) sg) ;if goal state has been reached
		 (if stats?
		     (return-from dfs (cons (remove nil (solution n))
					    `(,node-count 
					      ,redundancy
					      ,(length ahead)
					      ,(length behind))))
		     (return-from dfs (remove nil (solution n)))))
					;collect n's child nodes into a list
	     (setf children (remove nil (dfs-succ n `(,#'north ,#'south ,#'east ,#'west))))
	     (setf node-count (+ node-count (length children)))
	     (let ((temp (length children)))
	       (setf children ;get rid of nodes with states we already have/will cover
		     (diff children (append ahead behind)))
	       (setf redundancy (+ redundancy (- temp (length children)))))
	     (setf ahead (append children ahead))))))) ;add newly generated nodes to frontier
  
(defun iddfs (s0 sg d inc stats?)
  (let ((search (dfs s0 sg d stats?)))
    (if (equal search "no goal state found :(")
	(iddfs s0 sg (+ d inc) inc stats?)
	search)))