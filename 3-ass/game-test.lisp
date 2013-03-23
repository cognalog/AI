(defvar *state*)
(setf *state* '(((x - - - - - - -) 
		 (- - - - - - - -) 
		 (- - - - - - - -)
		 (- - - - - - - -) 
		 (- - - - - - - -) 
		 (- - - - - - - -) 
		 (- - - - - - - -) 
		 (- - - - - - - o)) 
		(x 1 1) (o 8 8)))

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
(defun p-row (player) (second player))
(defun p-col (player) (third player))

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

;compares my number of available moves to opponent's
(defun relative-flex (pl state)
  (let ((opp (if (equal pl 'x) 'o 'x)))
    (- (length (append (plus pl state) (times pl state)))
       (length (append (plus opp state) (times opp state))))))

(defun utility (pl state)
  (relative-flex pl state))
	  
(defun successors (pl parent)
  (let ((new-states nil) 
	(old-time (- (stop-time parent) (get-internal-real-time)))
	(new-time 0)
	(time-inc 0))
    (setf new-states
	  (append
	   (plus pl (state parent))
	   (times pl (state parent))))
    (setf time-inc (/ old-time (length new-states)))
    (setf new-time (+ (get-internal-real-time) time-inc))
    (mapcar
     (lambda (state)
       (let ((node (list state 0 new-time)))
	 (setf new-time (+ new-time time-inc))
	 node))
     new-states)))

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
	   (player 'o state)))
      (progn (format t "Illegal move by ~a" pl) nil)))