(load "search-aux.lisp")

(defun ucs-succ (node ops)
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
				     ,(+ (g-hat node) 1)))))
			      s-states))))
    s-nodes))

(defun update (kids ahead)
  (let ((curr-node nil) (sub-open nil))
    (loop 
       (if (null kids) (return-from update ahead))
       (setf curr-node (pop kids))
       (setf sub-open(member-state (state curr-node) ahead))
       (if sub-open
	   (let ((old-node (first sub-open)))
	     (if (< (g-hat curr-node) (g-hat old-node))
		 (setf (first sub-open) curr-node)))
	   (push curr-node ahead)))))

(defun ucs (s0 sg stats?)
  (let ((ahead `((,s0 ,nil ,nil ,0))) ;list of unexplored nodes, starting with s0
	(behind nil) ;list of explored nodes
	(n nil)
	(children nil)
	(node-count 1)
	(redundancy 0))
    (loop
       (if (null ahead) (return-from ucs "no goal state found :("))
       (setf n (pop ahead)) ;set N to first explored node
       (if (equal (state n) sg) ;if goal state has been reached
	   (if stats?
	       (return-from ucs (cons (remove nil (solution n))
				      `(,node-count 
					,redundancy
					,(length ahead)
					,(length behind))))
	       (return-from ucs (remove nil (solution n)))))
       (push n behind)
					;collect n's child nodes into a list
       (setf children (remove nil (ucs-succ n `(,#'north ,#'south ,#'east ,#'west))))
       (setf node-count (+ node-count (length children)))
					;get rid of nodes with states we already have covered
       (let ((temp (length children)))
	 (setf children (diff children behind))
	 (setf redundancy (+ redundancy (- temp (length children)))))
       (setf ahead (update children ahead))
       (setf ahead
	     (sort ahead (lambda (n1 n2) (< (g-hat n1) (g-hat n2))))))))