;; (load "serv.lisp")

(load "generator.lisp")

(defun styles ()
    (tag "style"
        "@import url('https://fonts.googleapis.com/css2?family=Assistant:wght@500&family=Open+Sans:wght@700&family=Poppins&family=Roboto+Mono:wght@100&display=swap');
        body { font-family: Poppins, sans-serif; background-color: #B1B2FF; }
        p, label, input[type='submit'] { font-size: 1.25rem; }
        main { background-color: #D2DAFF; padding: 20px; margin: auto; width: 40em; border-radius: 30px; text-align: center; }
        input { border-radius: 10px; padding: 15px; border: 2px solid #A7D2CB; max-width: 30em; width: 100%; }
        input[type='submit'] { background: #ADD8E6; max-width: 200px; border: none; }
        input[type='submit']:hover { background: green; cursor: pointer; }
        label { font-weight: 600; }
        h1 { background-color: #404040; padding: 10px; border-radius: 10px; font-size: 2rem; color: #90EE90; text-decoration: dotted hotpink underline 3px; }"))

(defun formfield (label id type placeholder)
    (concatenate 'string
        (tag "label" label (attr "id" id))
        (tag "br" "")
        (tag "input" "" (list (attr "type" type) (attr "name" id) (attr "placeholder" placeholder)))
        (tag "br" "")))

(defun index ()
    (tag "html"
        (list
            (tag "head"
                (list
                    (tag "title" "MediaWiki Sparkline Generator")
                    (styles)))
            (tag "body"
                (tag "main"
                    (list
                        (tag "h1" "MediaWiki Sparkline Generator")
                        (tag "p" "Generate a sparkline to visualise your MediaWiki contributions.")
                        (tag "h2" "Example")
                        (tag "p" "This is an example of a sparkline generated for a user on the IndieWeb wiki:")
                        (tag "embed" "" (list (attr "src" "https://sparkline.jamesg.blog/?username=Jamesg.blog&api_url=https://indieweb.org/wiki/api.php&only_image=true&days=30")))
                        (tag "h2" "Generate Your Own")
                        (tag "form"
                            (list
                                (formfield "MediaWiki API URL:" "api_url" "url" "https://en.wikipedia.org/w/api.php")
                                (formfield "Username (case-sensitive):" "username" "text" "ExampleName")
                                (tag "input" "" (list (attr "type" "hidden") (attr "name" "only_image") (attr "value" "true")))
                                (tag "input" "" (list (attr "type" "submit") (attr "value" "Generate"))))
                            (list (attr "action" "https://sparkline.jamesg.blog/") (attr "method" "GET")))))))))

(writetofile "out/serv.html" (index))