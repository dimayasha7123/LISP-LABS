(defun listTransformation (x) (
	cons (cons (cadr x) (cons (cons (caddr x) nil) (cons (car x) nil))) nil
))
(listTransformation '(open close halph))


(defun maxOf3 (x y z) (
	cond ((and (>= x y) (>= x z)) x)
		 ((and (>= y x) (>= y z)) y)
		 (t z)
))
(maxOf3 -4 5 67)


(defun listFromNumber (x) (
	cons (* 5 x) (cons (car (cond ((>= x 0)(cons '+ nil)) (t (cons '- nil)))) nil)
))
(listFromNumber 3)
(listFromNumber -7)


(defun f (ar1 ar2) (
	cons (cond ((atom ar1) (cons ar1 nil)) (t ar1))
		 (cond ((atom ar2) (cons (cons ar2 nil) nil)) (t (cons ar2 nil)))
))
(f '(2 34 5) 4) 
(f 2 '(1 2 3))
(f '(3 4 5) '(1 2 3))


(defun predicat (x y) (
	cond ((>= x y) t)
		 (t nil)
))
(predicat 3 4)
(predicat 9 0)