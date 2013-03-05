(load "search-aux.lisp")
(defun s-hat (node) (fifth node))

(defun star-succ (node ops sg)
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
				     ,(+ 1 (g-hat node))
				     ,(+ 1 (g-hat node) (manhat s-state sg))))))
			      s-states))))
    s-nodes))

(defun stupdate (kids ahead)
  (let ((curr-node nil) (sub-open nil))
    (loop 
       (if (null kids) (return-from stupdate ahead))
       (setf curr-node (pop kids))
       (setf sub-open(member-state (state curr-node) ahead))
       (if sub-open
	   (let ((old-node (first sub-open)))
	     (if (< (s-hat curr-node) (s-hat old-node))
		 (setf (first sub-open) curr-node)))
	   (push curr-node ahead)))))

(defun a-star (s0 sg stats?)
;list of unexplored nodes, starting with s0
  (let ((ahead `((,s0 ,nil ,nil ,0 (manhat s0 sg))))
	(behind nil) ;list of explored nodes
	(n nil)
	(children nil)
	(node-count 1)
	(redundancy 0))
    (loop
       (if (null ahead) (return-from a-star "no goal state found :("))
       (setf n (pop ahead)) ;set N to first explored node
       (if (equal (state n) sg) ;if goal state has been reached
	   (if stats?
	       (return-from a-star (cons (remove nil (solution n))
				      `(,node-count 
					,redundancy
					,(length ahead)
					,(length behind))))
	       (return-from a-star (remove nil (solution n)))))
       (push n behind)
					;collect n's child nodes into a list
       (setf children (remove nil (star-succ n `(,#'north ,#'south ,#'east ,#'west) sg)))
					;get rid of nodes with states we already have covered
       (setf node-count (+ node-count (length children)))
       (let ((temp (length children)))
	 (setf children (diff children behind))
	 (setf redundancy (+ redundancy (- temp (length children)))))
       (setf ahead (stupdate children ahead))
       (setf ahead
	     (sort ahead (lambda (n1 n2) (< (s-hat n1) (s-hat n2))))))))