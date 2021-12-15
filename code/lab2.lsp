(defun swapFirstAndThird (x) (
	cons (caddr x) (cons (cadr x) (cons (car x) (cdddr x)))
))
(swapFirstAndThird '(1 2 3 4 5 6 7))


(defun rootOfTheForm (x) (
	list 'Koren (strcat x "-OI") 'stepeni
))
(defun rootOfTheForm (x) (
	list 'Koren x '-oi 'stepeni
))
(rootOfTheForm 5)


;нужно (setq a 'a) или запускать (list1 1 2 3 4)
(defun list1 (x1 x2 x3 x4) (
	cons x1 (cons x2 (cons x3 (cons x4 NIL)))
))
(list1 a b c d)


(defun triangleSquare (a b c) (
	sqrt (* (/ (+ a b c) 2) (- (/ (+ a b c) 2) a) (- (/ (+ a b c) 2) b) (- (/ (+ a b c) 2) c))
))
(triangleSquare 3 4 5)


(defun shiftToTheRightByOne (x) (
	cons (car (last x)) (butlast x)
))
(shiftToTheRightByOne '(1 2 3 4 5 6 7))








