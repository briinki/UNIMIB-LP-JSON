;;;; -*- Mode: Lisp -*-

;;;; Luca Brini 879459

(defun jsonparse (json-string)
  (let ((json-char-list
	 (remove-whitespaces (str-to-chr-list json-string))))
    (or (jsonparse-array json-char-list)
        (jsonparse-object json-char-list)
        (error "Error: invalid JSON string"))))

(defun jsonparse-array (json-char-list)
  (cond ((string-equal (car json-char-list) "[")
	 (if (string-equal (cadr json-char-list) "]")
	     (values (cons 'JSONARRAY '()) (cddr json-char-list))
	     (multiple-value-bind (elements char-list-rest)
		 (jsonparse-elements (cdr json-char-list))
	       (if (not (string-equal (car char-list-rest) "]"))
		   (error "Syntax Error: cannot find closing ]")
		   (values (cons 'JSONARRAY elements)
			   (cdr char-list-rest))))))))

(defun jsonparse-elements (json-char-list)
  (multiple-value-bind (element next-element-and-rest)
      (jsonparse-value json-char-list)
    (cond ((string-equal (car next-element-and-rest) ",")
	   (multiple-value-bind (next-element char-list-rest)
	       (jsonparse-elements (cdr next-element-and-rest))
	     (values (cons element next-element) char-list-rest)))
	  (T (values (list element) next-element-and-rest)))))

(defun jsonparse-object (json-char-list)
  (cond ((string-equal (car json-char-list) "{")
	 (if (string-equal (cadr json-char-list) "}")
	     (values (cons 'JSONOBJ '()) (cddr json-char-list))
	     (multiple-value-bind (members char-list-rest)
             	 (jsonparse-members (cdr json-char-list))
	       (if (not (string-equal (car char-list-rest) "}"))
		   (error "Syntax Error: cannot find closing }")
		   (values (cons 'JSONOBJ members)
			   (cdr char-list-rest))))))))

(defun jsonparse-members (json-char-list)
  (multiple-value-bind (pair next-pair-and-rest)
      (jsonparse-pair json-char-list)
    (cond ((string-equal (car next-pair-and-rest) ",")
	   (multiple-value-bind (next-pair char-list-rest)
	       (jsonparse-members (cdr next-pair-and-rest))
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

;;; This function parse the next value in the json-char-list
;;; It supports: Strings, Objects, Arrays, Numbers, Booleans and null
(defun jsonparse-value (json-char-list)
  (print (car json-char-list))
  (cond ((string-equal (car json-char-list) "\"") 
         (multiple-value-bind (str char-list-rest)
             (jsonparse-string (cdr json-char-list))
           (values (chr-list-to-str str) char-list-rest)))
        ((string-equal (car json-char-list) "{")
         (multiple-value-bind (json-object char-list-rest)
             (jsonparse-object json-char-list)
           (values json-object char-list-rest)))
	((string-equal (car json-char-list) "[")
         (multiple-value-bind (json-array char-list-rest)
             (jsonparse-array json-char-list)
           (values json-array char-list-rest)))
	((digit-char-p (car json-char-list))
	 (multiple-value-bind (json-num char-list-rest)
             (jsonparse-number json-char-list)
           (values json-num  char-list-rest)))
	((string-equal (car json-char-list) "t")
	 (values 'true (subseq json-char-list 4)))
	((string-equal (car json-char-list) "f")
	 (values 'false (subseq json-char-list 5)))
	((string-equal (car json-char-list) "n")
	 (values 'null (subseq json-char-list 4)))
        (T (error "Syntax Error: unknown value type"))))

(defun jsonparse-string (json-char-list)
  (cond ((string-equal (first json-char-list) "\"") 
         (values NIL (cdr json-char-list)))
        (T (multiple-value-bind (str char-list-rest)
	       (jsonparse-string (cdr json-char-list))
             (values (cons (car json-char-list) str)
		     char-list-rest)))))

(defun jsonparse-number (json-char-list)
  (multiple-value-bind (digits-char-list char-list-rest)
      (jsonparse-number-proxy json-char-list)
    (let ((json-number (digits-list-to-number digits-char-list)))
      (values json-number char-list-rest))))

(defun jsonparse-number-proxy (json-char-list)
  (multiple-value-bind (int decimals-and-rest)
      (jsonparse-number-integer json-char-list)
    (if (string-equal (car decimals-and-rest) ".")
	(multiple-value-bind (floating-number exponential-and-rest)
	    (jsonparse-number-floating int (cdr decimals-and-rest))
	  (if (or (string-equal (car exponential-and-rest) "e")
		  (string-equal (car exponential-and-rest) "E"))
	      (multiple-value-bind (exponential-number char-list-rest)
		  (jsonparse-number-exponential 
		   floating-number 
		   (cdr exponential-and-rest))
		(values exponential-number char-list-rest))
	      (values floating-number exponential-and-rest)))
	(if (or (string-equal (car decimals-and-rest) "e")
		(string-equal (car decimals-and-rest) "E"))
	    (multiple-value-bind (exponential-number char-list-rest)
		(jsonparse-number-exponential int
					      (cdr decimals-and-rest))
	      (values exponential-number char-list-rest))
	    (values int decimals-and-rest)))))

(defun jsonparse-number-integer (json-char-list)
  (cond ((or (null json-char-list)
	     (not (digit-char-p (car json-char-list))))
	 (values NIL json-char-list))
	(T (multiple-value-bind (digt char-list-rest)
	       (jsonparse-number-integer (cdr json-char-list))
	     (values (cons (car json-char-list) digt)
		     char-list-rest)))))

(defun jsonparse-number-floating (int json-char-list)
  (multiple-value-bind (decimals char-list-rest)
      (jsonparse-number-integer json-char-list)
    (values (append int '(#\.) decimals) char-list-rest)))

(defun jsonparse-number-exponential (current-number json-char-list)
  (multiple-value-bind (exponent char-list-rest)
      (jsonparse-number-integer json-char-list)
    (values (append current-number '(#\e) exponent) char-list-rest)))

(defun digits-list-to-number (digits-list)
  (multiple-value-bind (raw-integer-part raw-decimal-part)
      (split-list-at-element digits-list #\.)
    (print raw-integer-part)
    (if (not (null raw-decimal-part))
	(let* ((integer-number
		(digits-list-to-number-integer
		 (reverse raw-integer-part)))
	       (decimal-number
		(float (/ (digits-list-to-number-integer
			   (reverse raw-decimal-part))
			  (expt 10 (length raw-decimal-part))))))
	  (+ integer-number decimal-number))
	(digits-list-to-number-integer (reverse raw-integer-part)))))

(defun digits-list-to-number-integer (integer-digits-list
				      &optional (position 0))
  (cond ((or (null integer-digits-list)) 0)
	(T (let ((incremental-number
		  (digits-list-to-number-integer
		   (cdr integer-digits-list) (1+ position))))
	     (+ (* (digit-char-p (car integer-digits-list))
		   (expt 10 position))
		incremental-number)))))


;;; UTILS SECTION

;;; From string to char-list and viceversa
(defun str-to-chr-list (str)
  (coerce str 'list))

(defun chr-list-to-str (char-list)
  (coerce char-list 'string))

(defun split-list-at-element (lst target-element)
  (let ((sublist-after (member target-element lst)))    
    (if (not sublist-after)
	(values lst NIL)
	(values (subseq lst 0 (- (length lst) (length sublist-after)))
		(cdr sublist-after))))) ;; removing the target-element

;;; This function removes all the whitespaces from the json-char-list.
;;; When it encounters a start string char (\") it calls the function
;;; which will ignore all the whitespace chars in the string.
(defun remove-whitespaces (json-char-list)
  (let ((chr (car json-char-list)))
    (cond ((null json-char-list) json-char-list)
	  ((or (string-equal chr #\Space)
	       (string-equal chr #\Tab)
	       (string-equal chr #\Return)
	       (string-equal chr #\Newline))
	   (remove-whitespaces (cdr json-char-list)))
	  ((string-equal chr "\"")
	   (cons chr (remove-whitespaces-ignore
		      (cdr json-char-list))))
	  (t (cons chr (remove-whitespaces (cdr json-char-list)))))))

(defun remove-whitespaces-ignore (json-char-list)
  (let ((chr (car json-char-list)))
    (cond ((null json-char-list) (error "Error: syntax error"))
          ((and (string-equal chr "\"") 
		(string-equal (cadr json-char-list) "\""));checks for \"
           (cons chr (cons (cadr json-char-list) 
			   (remove-whitespaces-ignore
			    (cddr json-char-list)))))
	  ((string-equal chr "\"") 
	   (cons chr (remove-whitespaces (cdr json-char-list))))
	  (T (cons chr (remove-whitespaces-ignore
			(cdr json-char-list)))))))
