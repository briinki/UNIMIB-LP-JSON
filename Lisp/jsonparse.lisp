(defun string-to-char-list (str)
  (coerce str 'list))

(defun is-whitespace (chr)
	(or (string-equal chr " ")
			(string-equal chr "\t")
			(string-equal chr "\r")
			(string-equal chr "\n"))
)

(defun ignore-whitespace (tokens)
	(if (is-whitespace (first tokens))
		(ignore-whitespace (rest tokens))
		tokens))

(defun is-last-of (target-char char-list)
	(string-equal target-char (car (last char-list))))

(defun jsonparse-members (tokens) 
	(cond ((null tokens) '())))

(defun jsonparse-object (tokens)
  (cond ((null tokens) (error "Unmatched curly brackets"))
        ((is-last-of "}" tokens)
         (cons 'JSONOBJ (jsonparse-members (butlast tokens))))
				((is-whitespace (first tokens)) 
					(jsonparse-members (ignore-whitespace (rest tokens))))))

(defun jsonparse_dispatcher (tokens)
  "This function takes the list of chars of the json string.
	It basically \"dispatch the workflow\" based of the next char 
	in the list"
	(print tokens)
  (cond ((null tokens) (error "Unexpected end of input"))
				((string-equal (first tokens) "{") (jsonparse-object (rest tokens)))))

(defun jsonparse (JSONString)
  (let ((tokens (string-to-char-list JSONString)))
    (jsonparse_dispatcher tokens)))
