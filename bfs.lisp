;node accessor fxns
(defun state (node) (first node))
(defun action (node) (second node))
(defun parent (node) (third node))

(defun diff (l1 l2)
  (cond
    ((or (null l1) (null l2)) l1)
    ((find (car l1) l2) (diff (cdr l1) l2))
    (T (cons (car l1) (diff (cdr l1) l2)))))

(defun push-back (e ls)
  (append ls `(,e)))

(defun zero (n)
  (eq n 0))

(defun solution (n)
  (cond
    ((null n) nil)
    (T (append (solution (parent n)) `(,(state n))))))

(defun pos (e ls)
  (cond
    ((equal e (car ls)) 0)
    (T (+ 1 (pos e (cdr ls))))))

(defun getl (n ls)
  (cond
    ((zero n) (car ls))
    (T (getl (- n 1) (cdr ls)))))

(defun setl (n ls e)
  (cond
    ((null ls) ls)
    ((zero n) (cons e (setl (- n 1) (cdr ls) e)))
    (T (cons (car ls) (setl (- n 1) (cdr ls) e)))))

(defun grid-get (x y grid)
  (cond
    ((null grid) grid)
    ((zero y) (getl x (car grid)))
    (T (grid-get x (- y 1) (cdr grid)))))

(defun grid-set (x y grid e)
    (cond
      ((null grid) grid)
      ((zero y) (cons (setl x (car grid) e) (grid-set x (- y 1) (cdr grid) e)))
      (T (cons (car grid) (grid-set x (- y 1) (cdr grid) e)))))

(defun succ-fxn (node ops)
  (let ((s-nodes nil) (s-states nil) (n-state (state node)))
    (loop for op in ops
       do
	 (setf s-states (funcall op n-state))
	 (setf s-nodes 
	       (append s-nodes
		      (mapcar (lambda (s-state) `(,s-state ,op ,node)) s-states))))
    s-nodes))

;OPERATOR FUNCTIONS FOR N-PUZZLE

(defun north (parent)
  (let ((par parent) (child '()))
    (loop for row in par
       for y from 0 to  (length par)
       do
	 (if (member 0 row)
	     (if (zero y) (return-from north nil)))
	 (setf child (push-back row child))					
	 (loop for col in row
	    for x from 0 to (length row)
	    do
	      (if (zero col)
		  (progn
		    (setf child (grid-set x y child (grid-get x (- y 1) child)))
		    (setf child (grid-set x (- y 1) child 0)))))) ;no modification
       child))

(defun west (parent)
  (let ((par parent) (child '()))
    (loop for row in par
       for y from 0 to  (length par)
       do
	 (setf child (push-back row child))	
	 (loop for col in row
	    for x from 0 to (length row)
	    do
	      (if (null col)
		  (progn
		    (if (eq x 0) (return-from west nil)) ;can't go further left
		    (setf child (grid-set x y child (grid-get (- x 1) y child)))
		    (setf child (grid-set (- x 1) y child nil)))))) ;swap
    child))

(defun south (parent)
  (let ((par parent) (child '()))
    (loop for row in par
       for y from 0 to  (length par)
       do
	 (if (member nil row)
	     (if (eq y (- (length par) 1)) (return-from south nil)))
	 (setf child (push-back row child))				
	 (loop for col in row
	    for x from 0 to (length row)
	    do
	      (if (null col)
		  (progn
		    (setf child (grid-set x y par (grid-get x (+ y 1) par)))
		    (setf child (grid-set x (+ y 1) child nil))
		    (return-from south child)))))))

(defun east (parent)
  (let ((par parent) (child '()))
    (loop for row in par
       for y from 0 to  (length par)
       do
	 (setf child (push-back row child))	
	 (loop for col in row
	    for x from 0 to (length row)
	    do
	      (if (null col)
		  (progn
		    (if (eq x (- (length row) 1)) (return-from east nil)) ;can't go further left
		    (setf child (grid-set x y child (grid-get (+ x 1) y child)))
		    (setf child (grid-set (+ x 1) y child nil)))))) ;swap
    child))

(defun bfs (s0 sg kids)
  (let ((ahead `((,s0 ,nil ,nil))) ;list of unexplored nodes, starting with s0
	(behind nil) ;list of explored nodes
	(n nil)
	(children nil)) ;N
    (loop
       (if (null ahead) (return "no goal state found :("))
       (setf n (pop ahead)) ;set N to first unexplored node
       (push-back (state n) behind)
       (if (equal (state n) sg) ;if goal state has been reached
	   (print "Solution:")
	   (return (solution n)))
       (setf children (funcall kids n `(,#'north ,#'south ,#'east ,#'west)) ;collect n's child nodes into a list
       (setf children ;get rid of nodes we already have/will cover
	     (diff children (append ahead behind)))
       (setf ahead (append ahead children)))))) ;add newly generated nodes to frontier