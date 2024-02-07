(in-package :cl-html-readme-test)

;;
;; HTML Rendering
;;

(define-test test-html-rendering-1 ()
  (let ((doc '((heading (:name "H1")))))
    (let ((rendered-html (cl-html-readme-test::doc-to-html doc))
	  (expected-html "<h1>H1</h1>"))
      (assert-equal expected-html rendered-html))))

;;
;; Heading Rendering Hooks
;;

(define-test test-html-rendering-heading-hook-1 ()
  (let ((cl-html-readme:*get-heading-attributes*
	  (lambda (properties)
            (list
             :class (getf properties :class)
	     :style "STYLE"
	     :a nil
	     :b (list "")
	     :c :KEYWORD
	     :d ""
	     :e (getf properties :style)))))
    (let ((doc '((heading (:name "H1" :class "CLASS" :style "")))))
      (let ((rendered-html (cl-html-readme-test::doc-to-html doc))
	    (expected-html "<h1 class=\"CLASS\" style=\"STYLE\">H1</h1>"))
	(assert-equal expected-html rendered-html)))))


;;
;; Semantic Rendering Hooks
;;

(define-test test-html-rendering-semantic-hook-1 ()
  (let ((cl-html-readme:*get-semantic-attributes*
	  (lambda (properties)
            (list
             :class (getf properties :class)
	     :style "STYLE"
	     :a nil
	     :b (list "")
	     :c :KEYWORD
	     :d ""
	     :e (getf properties :style)))))
    (let ((doc '((semantic (:name "article" :class "CLASS" :style "")))))
      (let ((rendered-html (cl-html-readme-test::doc-to-html doc))
	    (expected-html "<article class=\"CLASS\" style=\"STYLE\"></article>"))
	(assert-equal expected-html rendered-html)))))

;;
;; TOC Rendering Hooks
;;

(define-test test-html-rendering-toc-hook-1 ()
  (let ((doc '((toc (:root "ROOT" :container "CONTAINER" :item "ITEM"))
	       (heading (:name "H1" :toc t)
		(heading (:name "H1.1" :toc t))
		(heading (:name "H1.2" :toc t)
		 (heading (:name "H1.2.1" :toc t))))
	       (heading (:name "H2" :toc t)))))
    (let ((cl-html-readme:*get-toc-root-attributes*
	    (lambda (properties)
              (list
               :class (getf properties :root))))
	  (cl-html-readme:*get-toc-container-attributes*
	    (lambda (properties)
              (list
               :class (getf properties :container))))
	  (cl-html-readme:*get-toc-item-attributes*
	    (lambda (properties)
              (list
               :class (getf properties :item)))))
      ;;
      ;; This stuff is horror :(
      ;; Lets do it for the sake of having a test
      ;,
      (let ((rendered-html
	      (cl-html-readme-test::doc-to-html doc))
	    (expected-html
	      (concatenate
	       'string
	       (concatenate
		;;
		;; TOC
		;;
		'string
		"<ul class=\"ROOT\">"
		(concatenate
		 'string
		 (concatenate
		 'string
		 "<li class=\"ITEM\"><a href=\"#H1\">H1</a>"
		 (concatenate
		  'string
		  "<ul class=\"CONTAINER\">"
		  (concatenate
		   'string
		   "<li class=\"ITEM\"><a href=\"#H1.1\">H1.1</a></li>"
		   (concatenate
		    'string
		    "<li class=\"ITEM\"><a href=\"#H1.2\">H1.2</a>"
		    (concatenate
		     'string
		     "<ul class=\"CONTAINER\">"
		     "<li class=\"ITEM\"><a href=\"#H1.2.1\">H1.2.1</a></li>"
		     "</ul>")
		    "</li>"))
		  "</ul>")
		 "</li>"
		 "<li class=\"ITEM\"><a href=\"#H2\">H2</a></li>")
		"</ul>"))
	       (concatenate
		;;
		;; CONTENT
		;;
		'string
		"<h1 id=\"H1\">H1</h1>"
		"<h2 id=\"H1.1\">H1.1</h2>"
		"<h2 id=\"H1.2\">H1.2</h2>"
		"<h3 id=\"H1.2.1\">H1.2.1</h3>"
		"<h1 id=\"H2\">H2</h1>"))))
	(assert-equal expected-html rendered-html)))))
