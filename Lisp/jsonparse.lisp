(load "./utils.lisp")

(defun jsonparse (json-string)
  (let ((json-char-list (remove-whitespaces (str-to-chr-list json-string))))
    (or (jsonparse-array json-char-list)
        (jsonparse-object json-char-list)
        (error "Error: invalid JSON string"))))

(defun jsonparse-array (json-char-list)
  (cond ((eql (car json-char-list) #\[))))

(defun jsonparse-object (json-char-list)
  (cond ((string-equal (car json-char-list) "{")
					(if (string-equal (cadr json-char-list) "}")
						(values (cons 'JSONOBJ '()) (cddr json-char-list))
						(multiple-value-bind (members rest-char-list)
             	(jsonparse-member (cdr json-char-list))
							(if (not (string-equal (car rest-char-list) "}"))
								(error "Syntax Error: cannot found closing }")
								(values (cons 'JSONOBJ members) (cdr rest-char-list))))))
        (T (error "ERRORE"))))

(defun jsonparse-member (json-char-list)
	(multiple-value-bind (pair next-pair-and-rest)
		(jsonparse-pair json-char-list)
		(cond ((string-equal (car next-pair-and-rest) ",")
						(multiple-value-bind (next-pair char-list-rest)
							(jsonparse-member (cdr next-pair-and-rest))
							(values (cons pair next-pair) char-list-rest)))
					(T (values (list pair) next-pair-and-rest)))))

(defun jsonparse-pair (json-char-list)
	(multiple-value-bind (pair-key raw-value-and-rest)
		(jsonparse-pair-key json-char-list)
		(if (string-equal (car raw-value-and-rest) ":")
			(multiple-value-bind (pair-value char-list-rest)
				(jsonparse-value (cdr raw-value-and-rest))
				(values (list pair-key pair-value) char-list-rest))
				(error "Syntax Error: missing a colon"))))

(defun jsonparse-pair-key (json-char-list)
	(if (not (string-equal (car json-char-list) "\""))
		(error "Syntax Error: cannot find a valid pair key")
		(multiple-value-bind (pair-key raw-value-and-rest)
				(jsonparse-string (cdr json-char-list))
				(values (chr-list-to-str pair-key) raw-value-and-rest))))

(defun jsonparse-value (lista)
  (cond ((string-equal (car lista) "\"") 
         (multiple-value-bind (stringa resto)
             (jsonparse-string (cdr lista))
           (values (coerce stringa 'string) resto)))
        ((string-equal (car lista) "{")
         (multiple-value-bind (jsonobj resto)
             (jsonparse-object lista)
           (values jsonobj resto)))
        (T (error "Syntax Error: unknown value type"))))

(defun jsonparse-string (json-char-list)
  (cond ((string-equal (first json-char-list) "\"") 
         (values NIL (cdr json-char-list)))
        (T (multiple-value-bind (str chr-list-rest)
               (jsonparse-string (cdr json-char-list))
             (values (cons (car json-char-list) str) chr-list-rest)))))


;;; UTILS SECTION

;;; From string to char-list and viceversa
(defun str-to-chr-list (str)
  (coerce str 'list))

(defun chr-list-to-str (char-list)
  (coerce char-list 'string))

;;; This function removes all the whitespaces from the json-char-list.
;;; When it encounters a start string char (\") it calls the function
;;; which will ignore all the whitespace chars in the string.
;;; There are multiple definitions of same whitespace char because I've
;;; notice that \Newline and \n are considered differet in some systems
(defun remove-whitespaces (json-char-list)
	(let ((chr (car json-char-list)))
		(cond ((null json-char-list) json-char-list)
					((or (string-equal chr " ")
							(string-equal chr "\t")
							(string-equal chr "\r")
							(string-equal chr "\n")
							(string-equal chr #\Space)
							(string-equal chr #\Tab)
							(string-equal chr #\Return)
							(string-equal chr #\Newline))
					(remove-whitespaces (cdr json-char-list)))
					((string-equal chr "\"")
						(cons chr (remove-whitespaces-ignore (cdr json-char-list))))
					(t (cons chr (remove-whitespaces (cdr json-char-list)))))))
  
(defun remove-whitespaces-ignore (json-char-list)
	(let ((chr (car json-char-list)))
	(cond ((null json-char-list) (error "Error: syntax error"))
        ((and (string-equal chr "\"") 
							(string-equal (cadr json-char-list) "\""));checks for \"
         	(cons chr (cons (cadr json-char-list) 
										 			(remove-whitespaces-ignore (cddr json-char-list)))))
				((string-equal chr "\"") 
					(cons chr (remove-whitespaces (cdr json-char-list))))
				(T (cons chr (remove-whitespaces-ignore (cdr json-char-list)))))))