(defun string-to-char-list (str)
  (coerce str 'list))

(defun char-list-to-string (char-list)
  (coerce char-list 'string))

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

(defun jsonparse-string (tokens &optional (acc '()))
  (if (string-equal (first tokens) "\"")
      (list (char-list-to-string (reverse acc)) (rest tokens))
    (jsonparse-string (rest tokens) (cons (first tokens) acc))))

;; JSON OBJECT
;; from Pair --> string ws ':' value
;; to (string value)

(defun check-colon (tokens) 
	(cond ((string-equal (first (ignore-whitespace tokens)) ":") (rest tokens))
				(T (error "Key without value"))))
(defun jsonparse-pair (tokens)
	(let ((key-and-rest (jsonparse-string tokens)))
		(list (car key-and-rest) 
			(car (jsonparse-value (check-colon (second key-and-rest)))))))

(defun check-comma (tokens) 
	(let ((tokens-no-ws (ignore-whitespace tokens)))
		(cond ((string-equal (first (tokens-no-ws)) ",") (rest tokens-no-ws))
					((null tokens-no-ws) '()))))

(defun jsonparse-members (tokens) 
  (cond ((null tokens) '())
        ((is-whitespace (first tokens))
         (jsonparse-members (ignore-whitespace (rest tokens))))
        ((string-equal (first tokens) "\"") 
         (jsonparse-pair (rest tokens)))
        (T (error ""))))

(defun jsonparse-object (tokens)
  (cond ((null tokens) (error "Unmatched curly brackets"))
        ((is-last-of "}" tokens)
         (list 'JSONOBJ (jsonparse-members (butlast tokens))))))

;; JSON ARRAY
(defun jsonparse-element (tokens)
	(cond ((null tokens) '())))

(defun jsonparse-elements (tokens)
  (cond ((is-whitespace (first tokens))
         (jsonparse-elements (ignore-whitespace (rest tokens))))
        (T (jsonparse-element (rest tokens)))))

(defun jsonparse-array (tokens)
  (cond ((null tokens) (error "Unmatched square brackets"))
        ((is-last-of "]" tokens)
         (cons 'JSONARRAY (jsonparse-elements (butlast tokens))))))

;; JSONPARSE DISPATCHER
(defun jsonparse-dispatcher (tokens)
  (cond ((null tokens) (error "Unexpected end of input"))
				((string-equal (first tokens) "{") (jsonparse-object (rest tokens)))
				((string-equal (first tokens) "[") (jsonparse-array (rest tokens)))))

;; JSONPARSE
(defun jsonparse (JSONString)
  (let ((tokens (string-to-char-list JSONString)))
    (jsonparse-dispatcher tokens)))

;; JSONPARSE-VALUE DISPATCHER
(defun jsonparse-value (tokens)
	(let ((tokens-no-ws (ignore-whitespace (rest tokens))))
		(cond ((string-equal (first tokens-no-ws) "\"") 
						(jsonparse-string (rest tokens-no-ws)))
					((string-equal (first tokens-no-ws) "{")
						(jsonparse-object (rest tokens-no-ws)))
					((string-equal (first tokens-no-ws) "[")
						(jsonparse-array (rest tokens-no-ws)))
					((or (string-equal (first tokens-no-ws) "t")
							 (string-equal (first tokens-no-ws) "f")
							 (string-equal (first tokens-no-ws) "n"))
								 (jsonparse-constant tokens-no-ws)))))