;;;Tyrone Hinderson
;;;tph2107
;;;Assignment #4

(defvar *egs* (file-examples "~/Documents/AI/4-ass/restaurantlsp.txt" 12))

(defun remove* (e ls)
  (cond
    ((null ls) ls)
    ((equal e (car ls)) (remove* e (cdr ls)))
    ((listp (car ls)) (cons (remove* e (car ls)) (remove* e (cdr ls))))
    (t (cons (car ls) (remove* e (cdr ls))))))

(defun fmax (func ls)
  (let ((f (mapcar func ls)))
    (nth (position (apply #'max f) f :test #'equal) ls)))

(defun file-examples (filename)
  (let ((file (open filename)))
    (loop until (null (listen file)) collect
	 (read file))))

(defun get-class (eg)
  (car (last eg)))

(defun uni-class (egs)
  (cond
    ((eql (length egs) 1) t)
    ((equal (get-class (car egs))
	    (get-class (cadr egs)))
     (uni-class (cdr egs)))
    (t nil)))

(defun plurality (egs)
  (let ((class-list 
	 (loop for eg in egs collect
	      (get-class eg))))
    (fmax (lambda (class) 
	    (count class class-list))
	  class-list)))

(defun attribs-gen (egs)
  (let ((values '()))
    (loop for eg in egs do
	 (loop for i from 0 to (- (length eg) 1) do
	      (cond
		((eql i (length values))
		 (setf values (append values `((,i ,(nth i eg))))))
		((find (nth i eg) (nth i values)))
		(t (setf (nth i values) (append (nth i values) `(,(nth i eg))))))))
    values))

(defun subsets (att egs)
  (remove* nil (loop for val in (cdr att) collect
       (loop for eg in egs collect
	    (if (equal (nth (car att) eg) val)
		eg)))))

(defun entropy (egs)
  (let ((atts (attribs-gen egs)) (ent 0))
    (loop for val in (cdr (get-class atts)) do
	 (setf ent (+ ent
		    (* 
		     (/
		      (count val (mapcar #'get-class egs))
		      (length egs))
		     (log (/
			   (count val (mapcar #'get-class egs))
			   (length egs))
			  2)))))
    (- ent)))

(defun importance (att egs)
  (let ((remainder 0))
    (loop for sub in (subsets att egs) do
	 (setf remainder
	       (+ remainder
		  (*
		   (/ (length sub)
		      (length egs))
		   (entropy sub)))))
    (- (entropy egs) remainder)))

(defun dtl (egs attribs parent-eg)
  (cond
    ((null egs) (plurality parent-eg))
    ((uni-class egs) (get-class (car egs)))
    ((null attribs) (plurality egs))
    (t
     (let ((next-att (fmax (lambda (att) (importance att egs))
			   (remove* (car (last attribs)) attribs))))
       (let ((tree `(,(car next-att)))
	     (subs (subsets next-att egs)))
	 (loop for val in (cdr next-att)
	    for sub in subs do
	      (setf tree
		    (append tree 
			    `((,val ,(dtl sub 
					(remove* next-att attribs)
					egs))))))
	 tree)))))

(defun tree-to-func (tree)
  (cond
    ((null tree) nil)
    ((not (listp tree)) `(quote ,tree))
    (t `(cond
	    ,@(loop for val in (cdr tree) collect
		  `((equal (nth ,(car tree) eg) (quote ,(car val)))
		    ,(tree-to-func (cadr val))))
	    )))))
   

(defmacro learn (filename)
  (let ((egs (file-examples filename)))
    `#'(lambda (eg)
	   ,(tree-to-func (dtl egs (attribs-gen egs) egs)))))