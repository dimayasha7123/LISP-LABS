;��� �����. � ������ ����� ������ �������� �������� ������ �������� ������� (���������� �����). ������ : 
;���������� ������ : ��, ���������� ��������� ����� : ���, ����� : �������������, ��������� : ���������������.

;�������� �����
(filOpen 'fi "C:\filesForLisp\input.txt" _INPUT)

;�������� �����
(filClose 'fi)

;������ ����� �� �����, ��������� FILE, ������ ������ ����� ����� (���������)
(defun readToEnd (fil) (
	cond 	((fileof fil) nil)
			(t (cons (filGetLine fil) (readToEnd fil)))
))

;������� ���� ������ �� ��������
(defun split (str) (
	cond 	((= (strind str " ") 0) (cons str nil))
			(t (cons 	(strdel str (strind str " ") (strlen str))
						(split (strdel str 1 (strind str " ")))))
))

;��������� �������� �� ��������� ������ ����� �������� (��������� �������� ������)
(defun insertEnters (lst) (
	cond	((null lst) nil)
			(t (cons (car lst) (cons (strchr 13) (cons (strchr 10)(insertEnters(cdr lst))))))
))
(insertEnters '(("pip" "not" "found") ("please," "reinstall" "python") ("pip" "not" "found") ("please," "reinstall" "python")))

;������� ������ � ������������, �� ����� �������
(defun toFlatList (lst &optional (out nil)) (
	cond 	((null lst) out)
			((atom lst) (cons lst out))
			(t (toFlatList (car lst) (toFlatList(cdr lst) out)))
))
(toFlatList '(("pip" "not" "found") ("please," "reinstall" "python")))

;���� ������� ��� ��������� ������ ���� �����
(defun getStrsFromFile (fileName) (
	prog	(fi out)
			(filOpen 'fi fileName _INPUT)
			(setq out (toFlatList (insertEnters (mapcar (function split) (readToEnd 'fi)))))
			(filClose 'fi)
			(return out)
))		
(getStrsFromFile "C:\filesForLisp\input.txt")

(mapcar 'split '("privet" "pip not found" "please, reinstall python"))

;��������� ������� � ������������� ������ ����� � ��������� ���
(defun insertSpaces (lst) (
	cond 	((null lst) "")
			(t (strcat
				(car lst)
				(cond 
					((or 
						(equal (car lst) (strchr 13))
						(equal (car lst) (strchr 10))
						(null(cdr lst))) 
					 (insertSpaces (cdr lst)))
					(t (strcat (strchr 32) (insertSpaces (cdr lst)))))))
))

;��������		
(insertSpaces '("privet"
				"pip" "not" "found"
				"please," "reinstall" "python"))
				
;���������� ������ ����� � ����, ��������� ������� � ������ ������
;�� ���� ���� ���� �� ����� � ��� ������			
(defun writeStrsToFile (fileName lst) (
	prog	(fo)
			(filOpen 'fo fileName _OUTPUT)
			(filPutLine 'fo (insertSpaces lst))
			(filClose 'fo)
			(return "Completed!")
))		
(writeStrsToFile "C:\filesForLisp\output.txt"
				'("privet"
				"pip" "not" "found"
				"please," "reinstall" "python"))
				
		
;#1
(defun mapReplace (path forRep rep) (
	mapcar (function (lambda (str) (strrep str forRep rep))) (getStrsFromFile path)
))

;����� �� �����
(mapReplace "C:\filesForLisp\input.txt" "p" "PPP")

;����� � ����
(writeStrsToFile "C:\filesForLisp\output.txt" 
	(mapReplace "C:\filesForLisp\input.txt" "p" ""))


;������� ��������� ������ str, ���� ������� ���� � ������ ends
(defun replaceEnd (str ends) (
	cond	((null ends) str)
			((not (= 0 (strind (strcat str " ") (strcat (car ends) " "))))
			 (strrep (strcat str " ") (strcat (car ends) " ") ""))
			(t (replaceEnd str (cdr ends)))
))
(replaceEnd "henlo" '("qwe" "rty" "hen" "lo" "o"))

;#2
(defun endReplace (path ends) (
	mapcar (function (lambda (str) (replaceEnd str ends))) (getStrsFromFile path)
))

;����� �� �����
(endReplace "C:\filesForLisp\input.txt" '("vet" "fo" "on" "qwe"))

;����� � ����
(writeStrsToFile "C:\filesForLisp\output.txt" 
				(endReplace "C:\filesForLisp\input.txt" '("vet" "fo" "on" "qwe")))








