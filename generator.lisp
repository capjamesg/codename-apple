;; static site generator built in Lisp
;; codename: apple (that is the full name!)

(ql:quickload :markdown.cl-test)
(ql:quickload :cl-ppcre)
(ql:quickload :uiop)

;; generic HTML generation logic

(defun attr (name &optional value)
    ;; create an attribute key="value" pair
    (if value
        (concatenate 'string name "='" value "' ")
        (concatenate 'string name)))

(defun checkclosing (name)
    ;; check if a closing tag is needed
    (if (member name '(img br hr DOCTYPE))
        (concatenate 'string ">")
        (concatenate 'string "</" name ">")))

(defun tag (name &optional contents attributes)
    ;; convert contents and attribuets to flat lists of strings if a list is provided
    (if (listp contents)
        (setq contents (apply 'concatenate 'string contents)))
    (if (listp attributes)
        (setq attributes (apply 'concatenate 'string attributes)))

    (if (> (length attributes) 0) 
        (concatenate 'string "<" name " " attributes ">" contents (checkclosing name))
        (concatenate 'string "<" name ">" contents (checkclosing name))))

;; convenience functions for generating specific HTML tags

(defun rel (val url)
    ;; create a rel link
    (tag "link" "" (list (attr "rel" val) (attr "href" url))))

(defun link (text anchor &optional nop)
    ;; define an <a> link
    (if nop
        (tag "a" text (list (attr "href" anchor)))
        (tag "p" (tag "a" text (list (attr "href" anchor))))))

(defun writetofile (filename contents)
    (with-open-file (stream filename :direction :output :if-does-not-exist :create :if-exists :supersede)
        (format stream contents)))

(defun readfile (filename &optional newline)
    (with-open-file (in filename)
        (if newline
            (loop for line = (read-line in nil nil)
                while line collect (concatenate 'string line '(#\Newline)))
            (loop for line = (read-line in nil nil)
                while line collect line))))

(defun getpagetext (filestring)
    (loop for i from (+ (getlastpos filestring) 1) to (length filestring)
        collect (nth i filestring)))

(defun generateblogposts ()
    (loop for file in (remove-if-not (lambda (x) (search ".md" (namestring x))) (uiop:directory-files "../_posts/*.md"))
        collect (writetofile (concatenate 'string "out/" (pathname-name file) ".html") (blogpost (namestring file)))))

(defun generatepostlist ()
    (loop for file in (remove-if-not (lambda (x) (search ".md" (namestring x))) (uiop:directory-files "../_posts/*.md"))
        collect (frontmattertokeys (readfile (namestring file) t))))

(defun generatemapslist ()
    (loop for file in (remove-if-not (lambda (x) (search ".md" (namestring x))) (uiop:directory-files "../_checkins/*.md"))
        collect (frontmattertokeys (readfile (namestring file) t))))

;; likes.html
;; like page
;; bookmarks.html
;; bookmark page
;; notes.html
;; note page
;; photos.html
;; poll.html
;; projects.html
;; resume.html
;; replies.html
;; rome-churches.html
;; rsvps.html
;; sitemap.xml
;; tic-tac-toe1 2 3 4
;; travel 2021
;; travel 2022
;; travel.html
;; ukmap.html
;; category + tag pages
;; date archive pages



;; (writetofile "out/index.html" (home (reverse (generatepostlist))))
;; (writetofile "out/maps.html" (maphome (generatemapslist)))
;; (qr "GitHub")
;; (qr "Instagram")
;; (qr "LinkedIn")
;; (qr "Website")
;; (writetofile "out/checkin.html" (checkin (first (generatemapslist))))

;; (load "generator.lisp")