;;;; -*- Mode: Lisp -*-

;;;; Luca Brini 879459

(defun jsonparse (json-string)
  (let ((json-char-list
	 (remove-whitespaces (str-to-chr-list json-string))))
    (or (jsonparse-array json-char-list)
        (jsonparse-object json-char-list)
        (error "Error: invalid JSON string"))))


;;; JSONPARSE ARRAY
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


;;; JSONPARSE OBJECT
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


;;; JSONPARSE VALUE
;;; This function parse the next value in the json-char-list
;;; It supports: Strings, Objects, Arrays, Numbers, Booleans and null
(defun jsonparse-value (json-char-list)
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
	((or (digit-char-p (car json-char-list))
	     (string-equal (car json-char-list) "-"))
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

(defun jsonparse-number-integer (json-char-list
				 &optional (ignore-sign NIL))
  (cond ((or (string-equal (car json-char-list) "+")
	     (string-equal (car json-char-list) "-"))
	 (if (and  (not ignore-sign)
		   (not (null (cdr json-char-list))))
	     (multiple-value-bind (digit char-list-rest)
		 (jsonparse-number-integer (cdr json-char-list) T)
	       (values (cons (car json-char-list) digit)
		       char-list-rest))
	     (error
	      "Syntax Error: sign in not in the correct position")))
	((or (null json-char-list)
	     (not (digit-char-p (car json-char-list))))
	 (values NIL json-char-list))
	(T (multiple-value-bind (digt char-list-rest)
	       (jsonparse-number-integer (cdr json-char-list) T)
	     (values (cons (car json-char-list) digt)
		     char-list-rest)))))

(defun jsonparse-number-floating (int json-char-list)
  (multiple-value-bind (decimals char-list-rest)
      (jsonparse-number-integer json-char-list T)
    (values (append int '(#\.) decimals) char-list-rest)))

(defun jsonparse-number-exponential (current-number json-char-list)
  (multiple-value-bind (exponent char-list-rest)
      (jsonparse-number-integer json-char-list)
    (values (append current-number '(#\e) exponent) char-list-rest)))


;;; JSON ACCESS 
(defun jsonaccess (json-object &rest raw-fields)
  (let ((fields (flatten raw-fields)))
    (cond ((null fields) json-object)
	  ((eq (car json-object) 'JSONOBJ)
	   (jsonaccess-object (cdr json-object) fields))
	  ((eq (car json-object) 'JSONARRAY)
	   (jsonaccess-array (cdr json-object) fields))
	  (T (error "JSON Access Error: unknown object type")))))

(defun jsonaccess-object (json-object fields)
  (cond ((null json-object) NIL)
	((not (stringp (car fields)))
	 (error "JSON Access Error: invalid field"))
	((= (length fields) 1)
	 (jsonaccess-object-one-field json-object fields))
	(T (if (not (string-equal (caar json-object)
				  (car fields)))
	       (jsonaccess-object (cdr json-object) fields)
	       (jsonaccess (cadar json-object) (cdr fields))))))

(defun jsonaccess-object-one-field (json-object field)
  (cond ((null json-object) NIL)
	(T (if (not (string-equal (caar json-object)
				  (car field)))
	       (jsonaccess-object-one-field (cdr json-object) field)
	       (cadar json-object)))))

(defun jsonaccess-array (json-array indexes)
  (cond ((or (>= (car indexes) (length json-array))
	     (minusp (car indexes)))
	 (error "JSON Access Error: index out of bounds"))
	((not (numberp (car indexes)))
	 (error "JSON Access Error: invalid field"))
	(T (jsonaccess (nth (car indexes) json-array)
		       (cdr indexes)))))


;;; JSONREAD
(defun jsonread (filename)
  (if (or (null filename)
	  (string= filename ""))
      (error "JSON Read: invalid filename. Filename can't be nullish")
      (with-open-file (stream filename
			      :if-does-not-exist :error
			      :direction :input)
	(jsonparse (read-char-by-char stream))))) ;read

(defun read-char-by-char (stream)
  (let ((chr (read-char stream NIL NIL)))
    (cond ((null chr) chr)
          (T (cons chr (read-char-by-char stream))))))


;;; JSONDUMP
(defun jsondump (json-object filename)
  (if (or (null filename)
	  (string= filename ""))
      (error "JSON Dump: invalid filename. Filename can't be nullish")
      (with-open-file (out filename
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
	(format out "~A"
		(chr-list-to-str (jsonreverse json-object 1)))
	filename)))

(defun jsonreverse (json-object depth)
  (flatten 
   (cond ((eq (car json-object) 'JSONOBJ)
	  (list #\{ #\Newline
		(jsonreverse-object (cdr json-object) depth)
		(jsonreverse-tab-dispenser (1- depth)) #\}))
	 ((eq (car json-object) 'JSONARRAY)
	  (list #\[ #\Newline
		(jsonreverse-array (cdr json-object) depth)
		(jsonreverse-tab-dispenser (1- depth)) #\]))
	 (T (error "JSON Reverse Error: unknown object type ")))))

(defun jsonreverse-object (json-members depth)
  (if (null (car json-members))
      #\Space
      (list
       (jsonreverse-tab-dispenser depth)
       (jsonreverse-string (caar json-members))
       #\Space #\: #\Space
       (jsonreverse-value (cadar json-members) depth)
       (if (not (null (cdr json-members)))
	   (list #\, #\Newline
		 (jsonreverse-object (cdr json-members) depth))
	   #\Newline))))

(defun jsonreverse-array (json-elements depth)
  (if (null (car json-elements))
      #\Space
      (list
       (jsonreverse-tab-dispenser depth)
       (jsonreverse-value (car json-elements) depth)
       (if (not (null (cdr json-elements)))
	   (list #\, #\Newline
		 (jsonreverse-array (cdr json-elements) depth))
	   #\Newline))))

(defun jsonreverse-value (json-value depth)
  (cond ((stringp json-value) (jsonreverse-string json-value))
	((numberp json-value)
	 (str-to-chr-list (write-to-string json-value)))
	((or (eq json-value 'true)
	     (eq json-value 'false)
	     (eq json-value 'null))
	 (jsonreverse-constant json-value))
	(T (jsonreverse json-value (1+ depth)))))

(defun jsonreverse-string (json-string)
  (list #\" (str-to-chr-list json-string) #\"))

(defun jsonreverse-constant (json-constant)
  (str-to-chr-list (string-downcase (string json-constant))))

(defun jsonreverse-tab-dispenser (depth)
  (if (> depth 0)
      (cons #\Tab (jsonreverse-tab-dispenser (1- depth)))))


;;; UTILS SECTION

;;; From string to char-list and viceversa
(defun str-to-chr-list (str)
  (coerce str 'list))

(defun chr-list-to-str (char-list)
  (coerce char-list 'string))

(defun flatten (lst)
  (cond ((null lst) nil)
        ((atom lst) (list lst))
        (t (append (flatten (car lst))
                   (flatten (cdr lst))))))

;;; These utility functions convert a digit-char-list to 
;;; real CL numbers. Floating, integers, and in-scientific
;;; notation numbers are supported
(defun digits-list-to-number (digits-list)
  (multiple-value-bind (raw-base-part raw-exponent-part)
      (split-list-at-element digits-list #\e)
    (if (not (null raw-exponent-part))
	(if (member #\. raw-exponent-part)
	    (error "Syntax Error: exponent in exponential notation
must be an integer")   
	    (let* ((base-number (digits-list-to-number-floating-or-int
				 raw-base-part))
		   (exponent-number (digits-list-to-number-integer
				     raw-exponent-part)))
	      (* base-number (expt 10 exponent-number))))
	(digits-list-to-number-floating-or-int raw-base-part))))

;;; This functions checks if the input digit-char-list contains a dot.
;;; If so, it gets the integer and decimal part. The decimals then
;;; are divided by 10^(length decimals) in order to get the real
;;; decimals value. Finally, integer part and decimals part are
;;; added together.
(defun digits-list-to-number-floating-or-int (digits-list)
  (multiple-value-bind (raw-integer-part raw-decimal-part)
      (split-list-at-element digits-list #\.)
    (if (not (null raw-decimal-part))
	(let* ((integer-number
		(digits-list-to-number-integer raw-integer-part))
	       (decimal-number
		(float (/ (digits-list-to-number-integer
			   raw-decimal-part)
			  (expt 10 (length raw-decimal-part))))))
	  (if (minusp integer-number)
	      (- integer-number decimal-number)
	      (+ integer-number decimal-number)))
	(digits-list-to-number-integer raw-integer-part))))

;;; This functions check if there's the sign. Then it gets the
;;; number value by calling the helper function. The final value
;;; is the number from the helper multiplied by -1 if sign was
;;; negative or by +1 if sign was positive or not present.
(defun digits-list-to-number-integer (input-digits-list)
  (multiple-value-bind (sign unsigned-digits-list)
      (cond ((digit-char-p (car input-digits-list))
	     (values 1 input-digits-list))
	    ((string-equal (car input-digits-list) "+")
	     (values 1 (cdr input-digits-list)))
	    ((string-equal (car input-digits-list) "-")
	     (values -1 (cdr input-digits-list))))
    (let ((reverse-unsigned-digits-list
	   (reverse unsigned-digits-list)))
      (* sign (digits-list-to-number-integer-helper
	       reverse-unsigned-digits-list)))))

;;; This function takes a reverse digit-char-list number and
;;; compute the real value. It uses the following mechanism:
;;; 123 = 1*10^2 + 2*10^1 + 3*10^0
(defun digits-list-to-number-integer-helper (digits-list
					     &optional (position 0))
  (cond ((or (null digits-list)) 0)
	(T (let ((incremental-number
		  (digits-list-to-number-integer-helper
		   (cdr digits-list) (1+ position))))
	     (+ (* (digit-char-p (car digits-list))
		   (expt 10 position))
		incremental-number)))))


;;; This functions split a list in two sublist at a certain
;;; element. If element is not in the list, then it returns the
;;; initial list and NIL. Otherwise, it returns the sublists
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
		(string-equal (cadr json-char-list) "\""))
           (cons chr (cons (cadr json-char-list) 
			   (remove-whitespaces-ignore
			    (cddr json-char-list)))))
	  ((string-equal chr "\"") 
	   (cons chr (remove-whitespaces (cdr json-char-list))))
	  (T (cons chr (remove-whitespaces-ignore
			(cdr json-char-list)))))))
