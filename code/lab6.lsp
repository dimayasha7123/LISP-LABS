;Дан текст. В каждом слове текста заменить заданную литеру заданной литерой (сочетанием литер). Пример : 
;Заменяемая литера : “б”, заменяющее сочетание литер : “ку”, слово : “абракадабра”, результат : “акуракадакура”.

;открытие файла
(filOpen 'fi "C:\filesForLisp\input.txt" _INPUT)

;закрытие файла
(filClose 'fi)

;чтение файла до конца, принимает FILE, вернет список строк файла (построчно)
(defun readToEnd (fil) (
	cond 	((fileof fil) nil)
			(t (cons (filGetLine fil) (readToEnd fil)))
))

;сплитит одну строку по пробелам
(defun split (str) (
	cond 	((= (strind str " ") 0) (cons str nil))
			(t (cons 	(strdel str (strind str " ") (strlen str))
						(split (strdel str 1 (strind str " ")))))
))

;вставляем переходы на следующую строку между списками (исходными строками текста)
(defun insertEnters (lst) (
	cond	((null lst) nil)
			(t (cons (car lst) (cons (strchr 13) (cons (strchr 10)(insertEnters(cdr lst))))))
))
(insertEnters '(("pip" "not" "found") ("please," "reinstall" "python") ("pip" "not" "found") ("please," "reinstall" "python")))

;сжимаем список в одноуровнеый, не теряя порядка
(defun toFlatList (lst &optional (out nil)) (
	cond 	((null lst) out)
			((atom lst) (cons lst out))
			(t (toFlatList (car lst) (toFlatList(cdr lst) out)))
))
(toFlatList '(("pip" "not" "found") ("please," "reinstall" "python")))

;одна функция для получения списка слов файла
(defun getStrsFromFile (fileName) (
	prog	(fi out)
			(filOpen 'fi fileName _INPUT)
			(setq out (toFlatList (insertEnters (mapcar (function split) (readToEnd 'fi)))))
			(filClose 'fi)
			(return out)
))		
(getStrsFromFile "C:\filesForLisp\input.txt")

(mapcar 'split '("privet" "pip not found" "please, reinstall python"))

;вставляет пробелы в одноуровневый список строк и склеивает его
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

;работает		
(insertSpaces '("privet"
				"pip" "not" "found"
				"please," "reinstall" "python"))
				
;записывает список строк в файл, расставив пробелы в нужных местах
;на вход идет путь до файла и сам список			
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

;вывод на экран
(mapReplace "C:\filesForLisp\input.txt" "p" "PPP")

;вывод в файл
(writeStrsToFile "C:\filesForLisp\output.txt" 
	(mapReplace "C:\filesForLisp\input.txt" "p" ""))


;удаляет окончание строки str, если таковое есть в списке ends
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

;вывод на экран
(endReplace "C:\filesForLisp\input.txt" '("vet" "fo" "on" "qwe"))

;вывод в файл
(writeStrsToFile "C:\filesForLisp\output.txt" 
				(endReplace "C:\filesForLisp\input.txt" '("vet" "fo" "on" "qwe")))








