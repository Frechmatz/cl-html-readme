(in-package :cl-html-readme-test)


;;
;; Set indentation levels
;;

(define-test test-set-heading-indentation-levels-1 ()
  (let ((doc '((heading (:name "H1" :toc t))
	       (heading (:name "H2" :toc t))
	       (heading (:name "H3" :toc t)))))
    (let ((updated-doc (cl-html-readme-public-dsl-compiler::set-heading-indentation-levels doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :indentation-level 0))
	      (heading (:name "H2" :toc t :indentation-level 0))
	      (heading (:name "H3" :toc t :indentation-level 0)))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))

(define-test test-set-heading-indentation-levels-2 ()
  (let ((doc '((heading (:name "H1" :toc t)
		(heading (:name "H1.1" :toc t))
		(heading (:name "H1.2" :toc t)
		 (heading (:name "H1.2.1" :toc t))))
	       (heading (:name "H2" :toc t)))))
    (let ((updated-doc (cl-html-readme-public-dsl-compiler::set-heading-indentation-levels doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :indentation-level 0)
	       (heading (:name "H1.1" :toc t :indentation-level 1))
	       (heading (:name "H1.2" :toc t :indentation-level 1)
		(heading (:name "H1.2.1" :toc t :indentation-level 2))))
	      (heading (:name "H2" :toc t :indentation-level 0)))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))

;;
;; Set Heading-Ids
;;

(define-test test-set-heading-ids-1 ()
  (let ((doc '((heading (:name "H1" :toc t))
	       (heading (:name "H2" :toc t))
	       (heading (:name "H3" :toc t)))))
    (let ((updated-doc (cl-html-readme-public-dsl-compiler::set-heading-ids doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :id "H1"))
	      (heading (:name "H2" :toc t :id "H2"))
	      (heading (:name "H3" :toc t :id "H3")))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))

(define-test test-set-heading-ids-2 ()
  "Nested headings"
  (let ((doc '((heading (:name "H1" :toc t)
		(heading (:name "H1.1" :toc t)))
	       (heading (:name "H2" :toc t)))))
    (let ((updated-doc (cl-html-readme-public-dsl-compiler::set-heading-ids doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :id "H1")
	       (heading (:name "H1.1" :toc t :id "H1.1")))
	      (heading (:name "H2" :toc t :id "H2")))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))

(define-test test-set-heading-ids-3 ()
  "Heading names not unique"
  (let ((doc '((heading (:name "H1" :toc t))
	       (heading (:name "H1" :toc t))
	       (heading (:name "H1" :toc t))
	       (heading (:name "H2" :toc t)))))
    (let ((updated-doc (cl-html-readme-public-dsl-compiler::set-heading-ids doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :id "H1"))
	      (heading (:name "H1" :toc t :id "H1-1"))
	      (heading (:name "H1" :toc t :id "H1-2"))
	      (heading (:name "H2" :toc t :id "H2")))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))

(define-test test-set-heading-ids-4 ()
  "Headings declaring id => Id to be ignored"
  (let ((doc '((heading (:name "H1" :toc t :id "XXX"))
	       (heading (:name "H1" :toc t :id "YYY"))
	       (heading (:name "H2" :toc t)))))
    (let ((updated-doc (cl-html-readme-public-dsl-compiler::set-heading-ids doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :id "H1"))
	      (heading (:name "H1" :toc t :id "H1-1"))
	      (heading (:name "H2" :toc t :id "H2")))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))

(define-test test-set-heading-ids-5 ()
  "Non-Toc headings"
  (let ((doc '((heading (:name "H1" :toc t))
	       (heading (:name "H2"))
	       (heading (:name "H3" :toc t)))))
    (let ((updated-doc (cl-html-readme-public-dsl-compiler::set-heading-ids doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :id "H1"))
	      (heading (:name "H2" :id "H2"))
	      (heading (:name "H3" :toc t :id "H3")))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))


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
             :class (cl-html-readme-test::get-app-property properties :class)
	     :style "STYLE"
	     :a nil
	     :b (list "")
	     :c :KEYWORD
	     :d ""
	     :e (cl-html-readme-test::get-app-property properties :style)))))
    (let ((doc '((heading (:name "H1" :app (:class "CLASS" :style ""))))))
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
             :class (cl-html-readme-test::get-app-property properties :class)
	     :style "STYLE"
	     :a nil
	     :b (list "")
	     :c :KEYWORD
	     :d ""
	     :e (cl-html-readme-test::get-app-property properties :style)))))
    (let ((doc '((semantic (:name "article" :app (:class "CLASS" :style ""))))))
      (let ((rendered-html (cl-html-readme-test::doc-to-html doc))
	    (expected-html "<article class=\"CLASS\" style=\"STYLE\"></article>"))
	(assert-equal expected-html rendered-html)))))

;;
;; TOC Rendering Hooks
;;

(define-test test-html-rendering-toc-hook-1 ()
  (let ((doc '((toc (:app (:root "ROOT" :container "CONTAINER" :item "ITEM")))
	       (heading (:name "H1" :toc t)
		(heading (:name "H1.1" :toc t))
		(heading (:name "H1.2" :toc t)
		 (heading (:name "H1.2.1" :toc t))))
	       (heading (:name "H2" :toc t)))))
    (let ((cl-html-readme:*get-toc-root-attributes*
	    (lambda (properties)
              (list
               :class (cl-html-readme-test::get-app-property properties :root))))
	  (cl-html-readme:*get-toc-container-attributes*
	    (lambda (properties)
              (list
               :class (cl-html-readme-test::get-app-property properties :container))))
	  (cl-html-readme:*get-toc-item-attributes*
	    (lambda (properties)
              (list
               :class (cl-html-readme-test::get-app-property properties :item)))))
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
