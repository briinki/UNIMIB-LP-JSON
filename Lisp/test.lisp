(defparameter json "{\"nome\" : \"cognome\"}")

(defun string-index-of (element str &optional (index 0))
    (print str)
    (print index)
    (let ((string-length (length str)))
        (cond ((> index string-length) -1)
            ((string-equal (string (char str index)) (string element)) index)
            ((= string-length 0) -1)
            (T (string-index-of element (subseq str 1) (1+ index))))))

