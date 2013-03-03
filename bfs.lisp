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

(defun get-0-x (state) (cadr (getl (- (length state) 1) state)))
(defun get-0-y (state) (car (getl (- (length state) 1) state)))

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
  (let ((x (get-0-x parent)) (y (get-0-y parent)) (child parent))
    (if (zero y) (return-from north nil))
    (setf child (grid-set x y child (grid-get x (- y 1) child)))
    (setf child (grid-set x (- y 1) child 0)) ;no modification
    (setf (car (getl (- (length child) 1) child)) (- y 1))
    child))

(defun west (parent)
  (let ((x (get-0-x parent)) (y (get-0-y parent)) (child parent))
    (if (zero x) (return-from west nil))
    (setf child (grid-set x y child (grid-get (- x 1) y child)))
    (setf child (grid-set (- x 1) y child 0)) ;no modification
    (setf (cadr (getl (- (length child) 1) child)) (- x 1))
    child))

(defun south (parent)
  (let ((x (get-0-x parent)) (y (get-0-y parent)) (child parent))
    (if (eq y (- (length child) 2)) (return-from south nil))
    (setf child (grid-set x y child (grid-get x (+ y 1) child)))
    (setf child (grid-set x (+ y 1) child 0)) ;no modification
    (setf (car (getl (- (length child) 1) child)) (+ y 1))
    child))


(defun east (parent)
  (let ((x (get-0-x parent)) (y (get-0-y parent)) (child parent))
    (if (eq x (- (length (car child)) 1)) (return-from east nil))
    (setf child (grid-set x y child (grid-get (+ x 1) y child)))
    (setf child (grid-set (+ x 1) y child 0)) ;no modification
    (setf (cadr (getl (- (length child) 1) child)) (+ x 1))
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