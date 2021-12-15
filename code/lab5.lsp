;#2
(defun listLen (lst) (
	cond (lst (+ 1 (listLen (cdr lst))))
		 (t 0)
))

(defun addNewPair (lst &optional (num 1)) (
	cond (lst (cons (car lst) (addNewPair (cdr lst) (+ num 1))))
		 (t (cons (cons num (cons 0 nil)) nil))
))
 
(defun listInc (lst n) (
	cond ((<= (listLen lst) n) (listInc (addNewPair lst) n))
		 ((= n 0) (cons (cons (caar lst) (cons (+ 1 (cadar lst)) nil)) (cdr lst)))
		 (t (cons (car lst) (listInc (cdr lst) (- n 1))))
))
 
(defun nestListCount (lst &optional (lvl nil) (curr -1)) (
	cond ((null lst) lvl)
		 ((not (atom (car lst))) (nestListCount (cdr lst) (nestListCount (car lst) (listInc lvl (+ curr 1)) (+ curr 1)) curr))
		 (t (nestListCount (cdr lst) lvl curr))
)) 
(nestListCount '((1 2) 4 ((4 ((5))) 5)))

;#1
(defun nestedList (lst nested n) (
	cond (lst (or 
				(cond ((not (atom (car lst))) 
					(or
						(and 
							(equal (car lst) nested) 
							(= n 0)) 
						(nestedList (car lst) nested (- n 1))))
					 (t nil) )
				(nestedList (cdr lst) nested n)))
		 (t nil)))
(nestedList '((1 2) 2 4 5 1 (2 3 ((1 2)))) '(1 2) 0)

;#3
(defun setContains (lst x) (
	cond (lst (or (eq (car lst) x) (setContains (cdr lst) x)))
		 (t nil)
))
(setContains '(3 4 1 5 8 6) 2)

(defun setMinusElement (lst x) (
	cond ((not lst) nil)
		 ((= (car lst) x) (setMinusElement (cdr lst) x))
		 (t (cons (car lst) (setMinusElement (cdr lst) x)))
))

(defun setMinusSet (lst1 lst2) (
	cond ((not lst2) lst1)
		 (t (setMinusSet (setMinusElement lst1 (car lst2)) (cdr lst2)))
))
(setminusset '(3 5 6 1 8 9 12 2) '(1 2))

(defun isSetIncludeSubset (lst sublst) (
		cond 	(sublst	 (and 
							(setContains lst (car sublst)) 
							(isSetIncludeSubset lst (cdr sublst)))) 
				(t 		t) 
))
(isSetIncludeSubset '(3 5 6 1 8 9 12 2) '(1 2))
