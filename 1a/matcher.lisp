;you must load the whole file before the match function will work properly. Use the syntax (match p d) to test the primary function.  Lastly, the only variables available are ?a through ?z.
(defvar *poss*)
(defun match (p d)
  (when t
    (defparameter *poss* '() "the possible association lists")
    (if (null (matcher p d '())) nil
	(if  (null *poss*) T 
	     (if (equal 1 (length *poss*))
		 (car *poss*)
		 (killRunts (deDupe *poss*) (varCount *poss* 0)))))
    ))

(defun matcher (p d aList)
  (cond
    ((and (or (null p) (equal p '(*))) (null d)) ;base cases
     (if (null aList) T (when T (push (deDupe aList) *poss*) (deDupe aList))))
    ((or (null p) (null d)) nil)

    ((isVar (car p)) ;vars implemented
     (cond
       ((equal (varCheck (car p) (car d) aList) 'match) ; match in aList, move on
	(matcher (cdr p) (cdr d) aList))
       ((equal (varCheck (car p) (car d) aList) 'free) ; no match in aList, make new entry
	(matcher (cdr p) (cdr d) (cons `(,(car p) ,(car d)) aList)))
       (T (varCheck (car p) (car d) aList)))) ; must be nil

    ((or (equal (car p) '?) ; ? implemented
	 (equal (car p) (car d))) (matcher (cdr p) (cdr d) aList))

    ((and (listp (car p)) (listp (car d))) ; nesting implemented
     (handleNest (matcher (car p) (car d) aList) (matcher (cdr p) (cdr d) aList)))    

    ((equal (car p) '*) ; * implemented
     (handleKS (matcher (cdr p) d aList) (matcher  p (cdr d) aList)))

    (T nil)))

(defun handleKS (leaves takes)
;all this does is make sure both matchers are called
  (or leaves takes))

(defun handleNest (in out)
  (if (not (or (null in) (null out)))
      (cond
	((and (listp in) (listp out))
	 (push (dedupe (append in out)) *poss*))
	((listp in) in)
	((listp out) out)
	(T T))
      nil))

(defun killRunts  (ls min)
  (cond
    ((null ls) ls)
    ((< (length (car ls)) min) (killRunts (cdr ls) min))
    (t (cons (car ls) (killRunts (cdr ls) min)))))

(defun varCount (ls num)
  (cond
    ((null ls) num)
    ((<= (length (car ls)) num) (varCount (cdr ls) num))
    (t (varCount (cdr ls) (length (car ls))))))

(defun deDupe (ls)
  (cond
    ((null ls) ls)
    (t (cons (car ls) (deDupe (removeAll (car ls) (cdr ls)))))))

(defun removeAll (e ls)
  (cond
    ((null ls) ls)
    ((equal (car ls) e) (removeAll e (cdr ls)))
    (t (cons (car ls) (removeAll e (cdr ls))))))

(defun varCheck (var val aList)
  (cond
    ((null aList) 'free)
    ((equal var (car (car aList))) 
     (if (equal val (cadar aList)) 'match nil))
    (t (varCheck var val (cdr aList)))))

(defun isVar (el)
  (find el '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z)))