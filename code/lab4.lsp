;#1
(defun negative (x) (
	cond (x nil) (t t)
))

(defun equ (x y) (
	cond (x y) (t (negative y))
))

;#2
(defun bibanacci (x) (
	cond ((< x 3) (- x 1))
	     (t (+ (bibanacci (- x 1)) (bibanacci (- x 2))))
))

;#3
(defun supermegagigalastoflist (x) (
	cond ((negative (cdr x)) (car x))
		 (t (lastoflist (cdr x)))
))
(supermegagigalastoflist '(1 2 3))

;#4
(defun contains (x arr) (
	cond (arr (
			cond ((atom arr) (eq x arr))
				 (t (or (equal x arr) (contains x (car arr)) (contains x (cdr arr))))
		 ))
		 (t nil)
))

(contains '(1 3) '((1 2) 4 6 (5 (1 3))) )

;#5
(defun replace (lst obj1 obj2) (
	cond (lst (
			cond ((equal lst obj1) obj2)
				 ((atom lst) lst)
				 (t (cons (replace (car lst) obj1 obj2) (replace (cdr lst) obj1 obj2)))
		))
		(t nil)
))
(replace '((1 4) 9 8 4 1 (4 (1))) 1 3)

;#6
(defun isOneLevel (x) (
	cond ((not x) t)
		 (t (cond 
				((atom (car x)) (isOneLevel (cdr x)))
				(t nil)
))))

;#7
(defun countSumDeep (x) (
	cond ((atom x) (cond ((fixedp x) x) (t 0) ))
		 (t (+ (countSumDeep (car x)) (countSumDeep (cdr x))))
))
(countSumDeep '(1 ((2 3) 4 à û) 5 à í 6)) 

;#8
(defun firstAtom (x) (
	cond ((atom (car x)) (car x))
		 (t (or (cond ((not (atom (car x))) (firstAtom (car x))) (t nil)) 
				(firstAtom (cdr x))
		 ))
))