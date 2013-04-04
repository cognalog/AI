;Tyrone Hinderson, tph2107
(defvar *start*)
(setf *start* '(((x - - - - - - -) 
		 (- - - - - - - -) 
		 (- - - - - - - -)
		 (- - - - - - - -) 
		 (- - - - - - - -) 
		 (- - - - - - - -) 
		 (- - - - - - - -) 
		 (- - - - - - - o)) 
		(x 1 1) (o 8 8) null))

;get node/state/player info
(defun state (node) (first node))
(defun u-hat (node) (second node))
(defun stop-time (node) (third node))
(defun board (state) (first state))
(defun print-board (state) (format t "~{~{~a~^ ~}~%~}" (board state)))
(defun player (p state)
  (cond 
    ((equal p 'x) (second state))
    ((equal p 'o) (third state))
    (T nil)))
(defun opponent (pl) (if (equal pl 'x) 'o 'x))
(defun previous (state) (fourth state))
(defun p-row (player) (second player))
(defun p-col (player) (third player))

(defun rollback (state times)
  (cond
    ((or (null state) (eq times 0)) state)
    (t (rollback (previous state) (- times 1)))))

(defun getl (n ls)
  (cond
    ((null ls) ls)
    ((eq n 1) (car ls))
    (T (getl (- n 1) (cdr ls)))))

(defun setl (n ls e)
  (cond
    ((null ls) ls)
    ((eq n 1) (cons e (setl (- n 1) (cdr ls) e)))
    (T (cons (car ls) (setl (- n 1) (cdr ls) e)))))

(defun grid-get (row col grid)
  (cond
    ((null grid) grid)
    ((> row (length grid)) nil)
    ((eq row 1) (getl col (car grid)))
    (T (grid-get (- row 1) col (cdr grid)))))

(defun grid-set (row col grid e)
    (cond
      ((null grid) grid)
      ((> row (length grid)) nil)
      ((eq row 1) (cons (setl col (car grid) e) (grid-set (- row 1) col (cdr grid) e)))
      (T (cons (car grid) (grid-set (- row 1) col (cdr grid) e)))))

;determine whether a move is legal
(defun legal-move? (pl row col state)
  (let ((old-row (p-row (player pl state))) (old-col (p-col (player pl state))))
    (if (or (null row) (null col)) nil
	(and ;make sure space is free
	 (equal (grid-get row col (board state)) '-)
	 (or ;make sure it's a queen-move
	  (eq row old-row)
	  (eq col old-col)
	  (eq (abs (- row old-row))
	      (abs (- col old-col))))))))

;make a hypothetical move for a player if move is legal
(defun move (pl row col state)
  (if (legal-move? pl row col state)
      (list
       (grid-set row col 
		 (grid-set (p-row (player pl state))
			   (p-col (player pl state))
			   (board state)
			   '*);place the asterisk
		 pl);place the player
       (if (equal pl 'x) (list pl row col)
	   (player 'x state))
       (if (equal pl 'o) (list pl row col)
	   (player 'o state))
       state)
      (progn (format t "Illegal move ~a by ~a~%" `(,row ,col) pl) nil)))

;given a player and a game state, returns the possible non-diagonal moves
(defun plus (pl state)
  (let ((old-row (p-row (player pl state))) 
	(old-col (p-col (player pl state))))
    (append
     (loop for r from 1 
	until (null (legal-move? pl (- old-row r) old-col state))
	collect (move pl (- old-row r) old-col state))
     (loop for r from 1
	until (null (legal-move? pl (+ old-row r) old-col state))
	collect (move pl (+ old-row r) old-col state))
     (loop for c from 1 
	until (null (legal-move? pl old-row (- old-col c) state))
	collect (move pl old-row (- old-col c) state))
     (loop for c from 1
	until (null (legal-move? pl old-row (+ old-col c) state))
	collect (move pl old-row (+ old-col c) state)))))

;given a player and a game state, returns the possible diagonal moves
(defun times (pl state)
  (let ((old-row (p-row (player pl state))) 
	(old-col (p-col (player pl state))))
    (append
     (loop for d from 1 
	until (null (legal-move? pl (- old-row d) (- old-col d) state))
	collect (move pl (- old-row d) (- old-col d) state))
     (loop for d from 1
	until (null (legal-move? pl (+ old-row d) (+ old-col d) state))
	collect (move pl (+ old-row d) (+ old-col d) state))
     (loop for d from 1
	until (null (legal-move? pl (+ old-row d) (- old-col d) state))
	collect (move pl (+ old-row d) (- old-col d) state))
     (loop for d from 1
	until (null (legal-move? pl (- old-row d) (+ old-col d) state))
	collect (move pl (- old-row d) (+ old-col d) state)))))

;looks at diagonals or non-diag first at random
(defun possible-moves (pl state)
  (if (> (random 2) 0)
      (append (plus pl state) (times pl state))
      (append (times pl state) (plus pl state))))

;HEURISTICS

;compares my moves with opponent's
(defun relative-flex (pl state)
  (- 
   (* (length (possible-moves pl state)) 2)
   (length (possible-moves (opponent pl) state))))

;these two determine the amount of adjacency among asterices
(defun adjacent-asterices (row col state)
  (let ((total 0))
    (loop for i from (- row 1) to (+ row 1) do
	 (loop for j from (- col 1) to (+ col 1) do
	      (if (equal (grid-get i j (board state)) '*) (incf total))))
    total))

(defun asterisk-parties (state)
  (let ((grid (board state)))
    (apply #'+
	   (loop for r from 1 to (length grid) collect
		(apply #'+ 
		       (loop for c from 1 to (length (getl r grid)) collect
			    (if (equal (grid-get r c grid) '*)
				(adjacent-asterices r c state)
				0)))))))    

;find the area of freedom
(defun row-ast (row)
  (cond
    ((null row) 0)
    ((equal (car row) '*) (+ 1 (row-ast (cdr row))))
    (t (row-ast (cdr row)))))

(defun col-ast (col board)
  (cond
    ((null board) 0)
    ((equal (getl col (car board)) '*)
     (+ 1 (col-ast col (cdr board))))
    (t (col-ast col (cdr board)))))

(defun free-area (pl state)
  (let ((p-r (p-row (player pl state)))
	(p-c (p-col (player pl state)))
	(grid (board state))
	(n-bound 0) (s-bound (length (board state)))
	(e-bound (length (car (board state)))) (w-bound 0))
;define perimeter based on asterisk density
    (loop for r in grid
       for i from 1 to (length grid) do
	 (if (< i p-r)
	     (if (< (row-ast (getl n-bound grid))
		    (row-ast r))
		 (setf n-bound i)))
	 (if (> i p-r)
	     (if (< (row-ast (getl s-bound grid))
		    (row-ast r))
		 (setf s-bound i))))
    (loop for j from 1 to (length (car grid)) do
	 (if (< j p-c)
	     (if (< (col-ast w-bound grid)
		    (col-ast j grid))
		 (setf w-bound j)))
	 (if (> j p-c)
	     (if (< (col-ast e-bound grid)
		    (col-ast j grid))
		 (setf e-bound j))))
    (* (- s-bound n-bound)
       (- e-bound w-bound))))
    
(defun relative-freedom (pl state)
  (- (* (free-area pl state) 1.5)
     (free-area (opponent pl) state)))

(defun casual-ev (pl state)
  (let ((opp (opponent pl)) (moves (length (possible-moves pl state))))
    (cond
      ((eq moves 0) -999)
      ((eq (length (possible-moves opp state)) 0) 999)
      (t (+ (* (relative-flex pl state) 2)
	    (/ (asterisk-parties state) 4)
	    (relative-freedom pl state))))))

(defun careful-ev (pl state)
  (let ((opp (opponent pl)) (moves (length (possible-moves pl state))))
    (cond
      ((eq moves 0) -999)
      ((eq (length (possible-moves opp state)) 0) 999)
      (t (+ (* (length (relative-flex pl state)) 4)
	    (* (relative-freedom pl state) 2))))))

(defun emergency? (pl state)
  (let ((p-r (p-row (player pl state)))
	(p-c (p-col (player pl state)))
	(o-r (p-row (player (opponent pl) state)))
	(o-c (p-col (player (opponent pl) state))))
    (let ((enemy-close (or (eq (abs (- p-r o-r)) 1)
			   (eq (abs (- p-c o-c)) 1)))
	  (ast-prox (>= (adjacent-asterices p-r p-c state) 2))
	  (few-moves (< (length (possible-moves pl state)) 10))
	  (on-edge (or (eq p-r 8) (eq p-c 8) (eq p-r 1) (eq p-c 1))))
    (or
     (and enemy-close ast-prox)
     (and enemy-close few-moves)
     (and ast-prox few-moves)
     (and enemy-close on-edge)))))

;check to see whether a node cannot have any more children
(defun terminal? (node)
  (or
;out of time
   (>= (get-internal-real-time) (stop-time node))
;out of moves
   (eq (length (possible-moves 'x (state node))) 0)
   (eq (length (possible-moves 'o (state node))) 0)))

(defun successors (pl parent)
  (let ((new-states nil)
;old time: the amount of time left in the turn 
	(old-time (- (stop-time parent) (get-internal-real-time)))
	(new-time 0)
	(time-inc 0))
    (setf new-states (possible-moves pl (state parent)))
    (setf time-inc (/ old-time (length new-states)))
    (setf new-time (+ (get-internal-real-time) time-inc))
    (mapcar
     (lambda (state)
       (let ((node (list state 0 new-time)))
					;update stop-time for next search path
	 (setf new-time (+ new-time time-inc))
	 node))
     new-states)))

(defun max-value (pl node alpha beta depth)
  (if (emergency? pl (state node))
      (return-from max-value (careful-ev pl (state node))))
  (if (or (eq depth 0) (terminal? node))
      (return-from max-value (casual-ev pl (state node))))
  (let ((value -999))
    (loop for s in (successors pl node) do
	 (setf value (max value (min-value pl s alpha beta (- depth 1))))
	 (if (>= value beta) (return-from max-value value))
	 (setf alpha (max alpha value)))
    value))

(defun min-value (pl node alpha beta depth)
  (if (emergency? pl (state node))
      (return-from min-value (careful-ev pl (state node))))
  (if (or (eq depth 0) (terminal? node))
      (return-from min-value (casual-ev pl (state node))))
  (let ((value 999))
    (loop for s in (successors pl node) do
	 (setf value (min value (max-value pl s alpha beta (- depth 1))))
	 (if (<= value alpha) (return-from min-value value))
	 (setf beta (min beta value)))
    value))

(defun best-move (pl state secs)
  (let ((kids (successors
	       pl 
	       (list state 0 
		     (+ (get-internal-real-time)
			(* secs 1000))))))
    (let ((values (mapcar (lambda (node) (min-value pl node -999 999 12)) kids)))
      (cdr (player pl (state (getl (+ (position (apply #'max values) values :test #'eq) 1)
				   kids)))))))

(defun main (pl state secs turn)
  (if (equal turn pl)
;it is our move
      (let ((b-m (best-move pl state secs))
	    (n-state nil) 
	    (opp (if (equal pl 'x) 'o 'x)))
	(if (or (null (car b-m)) (null (cadr b-m)))
	    (format t "I surrender ~a~%~a~%" b-m (print-board state))
	    (progn
	      (setf n-state (move pl (car b-m) (cadr b-m) state))
	      (format t "my move: ~a~%~a~%" b-m (print-board n-state))
	      (main pl n-state secs opp))))
;it is opponent's move
      (progn
	(print "What is the opponent's move?")
	(let ((opp (if (equal pl 'x) 'o 'x)) 
	      (o-m (read))
	      (n-state nil))
	  (cond
	      ((equal o-m 'stop) (return-from main 'done))
	      ((not (listp o-m))
		(format t "~%input must be in form (row column)~%")
		(main pl state secs turn))) 
	  (let ((row (car o-m)) (col (cadr o-m)))
	    (cond
	      ((equal row 'rollback) ;rollback is requested
	       (setf n-state (rollback state col))
	       (format t "Board has been rolled back ~a times~%~a~%" 
		       col (print-board n-state))
	       (if (evenp col)
		   (main pl n-state secs opp)
		   (main pl n-state secs pl)))
;opp gives bad move
	      ((or (equal o-m '(null null))
		   (not (legal-move? opp row col state)))
	       (format t "Opponent did not move, Victory is mine!~%~a" 
		       (print-board state))
	       (main pl state secs opp))
;opp gives normal move
	      (t (setf n-state (move opp row col state))
		 (format t "~a~%" (print-board n-state))
		 (main pl n-state secs pl))))))))

	   

(defun begin (secs)
  (format t "Which player am I? ('x' or 'o')")
  (let ((pl (read)))
    (cond
      ((equal pl 'x) 
       (let ((bm (best-move pl *start* secs)) (n-state nil))
	 (setf n-state (move pl (car bm) (cadr bm) *start*))
	 (format t "~a~%~a~%" bm (print-board n-state))
	 (main pl n-state secs 'o)))
      ((equal pl 'o) (main pl *start* secs 'x))
      (t (format t "dude, x or o.~%")
	 (begin secs)))))