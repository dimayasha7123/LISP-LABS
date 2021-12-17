(setq tr '(5 
	(3 
		(1 nil nil) 
		(4 nil nil)) 
	(7 
		(6 nil nil) nil)))

;пустое дерево
;(nil nil nil)

(defun data (tree) (
	cond	((or (null tree) (atom tree)) nil)
			(t (car tree))
))

(defun left (tree) (
	cond	((or (null tree) (atom tree)) nil)
			(t (cadr tree))
))

(defun right (tree) (
	cond	((or (null tree) (atom tree)) nil)
			(t (caddr tree))
))
	
(defun find (x tree) (
	cond	((or (null tree) (null (data tree)) (null x)) nil)
			((= x (data tree)) t)
			((< x (data tree)) (find x (left tree)))
			((> x (data tree)) (find x (right tree)))
))

(defun add (x tree) (
	cond 	((or (null tree) (null (data tree))) (list x nil nil))
			((= x (data tree)) tree)
			((< x (data tree)) (list (data tree) (add x (left tree)) (right tree)))
			((> x (data tree)) (list (data tree) (left tree) (add x (right tree))))
))

(defun prevTree (x tree) (
	cond	((null tree) nil)
			((<= x (data tree)) (prevTree x (left tree)))
			(t (list (data tree) (left tree) (prevTree x (right tree))))
))

(defun nextTree (x tree) (
	cond	((null tree) nil)
			((>= x (data tree)) (nextTree x (right tree)))
			(t (list (data tree) (nextTree x (left tree)) (right tree)))
))

(defun join (p q) (
	cond 	((null p) q)
			((null q) p)
			(t (list(data p)
					(join 	(left p)
							(prevTree (data p) q))
					(join	(right p)
							(nextTree (data p) q)) 
))))

(defun takeRight (tree) (
	cond	((null (right tree)) (data tree))
			(t (takeRight (right tree)))
))

(defun del (x tree) (
	cond	((null tree) nil)
			((= x (data tree)) (
				cond	((null (left tree)) (right tree))
						((null (right tree)) (left tree))
						(t (list	(takeRight (left tree))
								(del (takeRight (left tree)) (left tree))
								(right tree)))))
			((< x (data tree)) (list (data tree) (del x (left tree)) (right tree)))
			((> x (data tree)) (list (data tree) (left tree) (del x (right tree))))

))


(cond 
	((условие) (что делать))
	(() ())
	(t ())
)
	












