(in-package :cl-html-readme-test)

;;
;; Set Heading-Ids
;;

(define-test test-set-heading-ids-1 ()
  (let ((doc '((heading (:name "H1" :toc t))
	       (heading (:name "H2" :toc t))
	       (heading (:name "H3" :toc t)))))
    (let ((updated-doc (cl-html-readme-dsl-frontend::set-heading-ids doc))
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
    (let ((updated-doc (cl-html-readme-dsl-frontend::set-heading-ids doc))
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
    (let ((updated-doc (cl-html-readme-dsl-frontend::set-heading-ids doc))
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
    (let ((updated-doc (cl-html-readme-dsl-frontend::set-heading-ids doc))
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
    (let ((updated-doc (cl-html-readme-dsl-frontend::set-heading-ids doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :id "H1"))
	      (heading (:name "H2"))
	      (heading (:name "H3" :toc t :id "H3")))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))

