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

;; Split the string by ",". Then
;; if in a substring there is an odd number of "\""
;; it means that i've splitted a json-string.
;; if not, it means that i've splitted the pairs
;; O(n)
(defun split-string (delimiter tokens)
  ((lambda (split start end)
     (if end
         (let ((token (subseq tokens start end)))
           (cons token (split (1+ end) (position delimiter tokens :start (1+ end)))))
         NIL))
   (lambda (start end)
     (if end
         (let ((token (subseq tokens start end)))
           (cons token (split (1+ end) (position delimiter tokens :start (1+ end)))))
         NIL))
   0
   (position delimiter tokens)))


(defun split_pairs (tokens)
	"This function splits the members in a pair-list"
	(let (pair-list ))
)

;; members --> empty | pair, pairs
(defun jsonparse-members (tokens) 
	(cond ((null tokens) '())
		((is-single-pair tokens) (jsonparse-pair tokens))
		(T ())))

;; object -> { ws } | { members }
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
