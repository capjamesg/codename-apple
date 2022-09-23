;; get values from front matter
;; example:
;; ---
;; title: This is a test
;; ---
;; returns a hash table with a key "title" and a value "This is a test"
;; lists in front matter are defined by comma separated values
;; any front matter value with a comma in it will be treated as a list (until I make a fix to otherwise recognise lists)
;; author: capjamesg

(defun sub (str text)
    (substitute #\Space str text))

(defun cleantag (text)
    (remove #\] (remove #\[ (sub #\" text))))

(defun frontmattertokeys (filedata)
    (defparameter *keylist* (make-hash-table))
    (loop for item in (getfrontmatter filedata)
        do (if (or (find "," (value item) :test #'equal) (string= (key item) "categories"))
            (setf (gethash (read-from-string (key item)) *keylist*) (cl-ppcre:split ", " (string-trim '(#\Space) (cleantag (value item)))))
            (setf (gethash (read-from-string (key item)) *keylist*) (string-trim '(#\Space) (cleantag (value item))))))
    (if (not (gethash 'permalink *keylist*)) (setf (gethash 'permalink *keylist*) "/"))
    (if (not (gethash 'published *keylist*)) (setf (gethash 'published *keylist*) "January 1st, 2021"))
    ;; (print (getpagetext filedata))
    ;; (if (not (gethash 'description *keylist*)) (setf (gethash 'description *keylist*) (markdown:parse (second (getpagetext filedata)))))
    *keylist*)

(defun getpos (list)
    (loop for i from 0 to (length list)
        ;; search for #\Newline
        if (equal (nth i list) (concatenate 'string "---" '(#\Newline)))
            return i))

(defun getposstr (iter)
    (loop for i from 0 to (length iter)
        if (equal (char iter i) #\:)
            return i))

(defun getlastpos (list)
    (loop for i from (length list) downto 0
        if (equal (nth i list) (concatenate 'string "---" '(#\Newline)))
            return i))

(defun getfrontmatter (filestring)
    (loop for i from (+ (getpos filestring) 1) to (- (getlastpos filestring) 1)
        collect (nth i filestring)))

(defun value (pair)
   (subseq pair (+ (getposstr pair) 2)))

(defun key (pair)
   (subseq pair 0 (getposstr pair)))