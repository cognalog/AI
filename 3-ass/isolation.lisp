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
(defun previous (state) (fourth state))
(defun p-row (player) (second player))
(defun p-col (player) (third player))

(defun rollback (state times)
  (cond
    ((eq times 0) state)
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
   (and ;make sure space is free
    (equal (grid-get row col (board state)) '-)
    (or ;make sure it's a queen-move
     (eq row old-row)
     (eq col old-col)
     (eq (abs (- row old-row))
	 (abs (- col old-col)))))))

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
      (progn (format t "Illegal move by ~a" pl) nil)))

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

(defun possible-moves (pl state)
  (append (plus pl state) (times pl state)))

(defun utility (pl state)
  (length (possible-moves  pl state)))

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

(defun max-value (pl node alpha beta)
  (if (terminal? node) (return-from max-value (utility pl (state node))))
  (let ((value -9999999))
    (loop for s in (successors pl node) do
	 (setf value (max value (min-value pl s alpha beta)))
	 (if (>= value beta) value)
	 (setf alpha (max alpha value)))
    value))

(defun min-value (pl node alpha beta)
  (if (terminal? node) (return-from min-value (utility pl (state node))))
  (let ((value 9999999))
    (loop for s in (successors pl node) do
	 (setf value (min value (max-value pl s alpha beta)))
	 (if (<= value alpha) value)
	 (setf beta (min beta value)))
    value))

(defun best-move (pl state secs)
  (let ((kids (successors
	       pl 
	       (list state 0 
		     (+ (get-internal-real-time)
			(* secs 1000))))))
    (let ((values (mapcar (lambda (node) (max-value pl node -999 999)) kids)))
      (cdr (player pl (state (getl (position (apply #'max values) values :test #'eq)
	    kids)))))))

(defun main (pl state secs)
  (print "What is the opponent's move?")
  (let ((opp (if (equal pl 'x) 'o 'x)) (o-m (read)) (n-state nil))
    (cond
      ((or (equal o-m '(null null))
	   (not (legal-move? opp (car o-m) (cadr o-m) state)))
       (format t "Victory is mine!~%~a" (print-board state)))
      ((equal (car o-m) 'rollback)
       (setf n-state (rollback state (cadr o-m)))
       (format t "Board has been rolled back ~a~%~a~%" 
	       (cadr o-m) (print-board n-state))
       (main pl n-state secs))
      (t (setf n-state (move opp (car o-m) (cadr o-m) state))
	 (let ((b-m (best-move pl n-state secs)))
	   (setf n-state (move pl (car b-m) (cadr b-m) n-state))
	   (format t "~a~%~a~%" b-m (print-board n-state)))
	 (main pl n-state secs)))))
	   

(defun begin (secs)
  (format t "Which player am I? ('x' or 'o')")
  (let ((pl (read)))
    (cond
      ((equal pl 'x) 
       (let ((bm (best-move pl *start* secs)) (n-state nil))
	 (setf n-state (move pl (car bm) (cadr bm) *start*))
	 (format t "~a~%~a~%" bm (print-board n-state))
	 (main pl n-state secs)))
      ((equal pl 'o) (main pl *start* secs))
      (t (format t "dude, x or o~%")
	 (begin secs)))))