(in-package :cl-html-readme-test)


;;
;; Set indentation levels
;;

(define-test test-set-heading-indentation-levels-1 ()
  (let ((doc '((heading (:name "H1" :toc t))
	       (heading (:name "H2" :toc t))
	       (heading (:name "H3" :toc t)))))
    (let ((updated-doc (cl-html-readme::set-heading-indentation-levels doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :level 0))
	      (heading (:name "H2" :toc t :level 0))
	      (heading (:name "H3" :toc t :level 0)))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))

(define-test test-set-heading-indentation-levels-2 ()
  (let ((doc '((heading (:name "H1" :toc t)
		(heading (:name "H1.1" :toc t))
		(heading (:name "H1.2" :toc t)
		 (heading (:name "H1.2.1" :toc t))))
	       (heading (:name "H2" :toc t)))))
    (let ((updated-doc (cl-html-readme::set-heading-indentation-levels doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :level 0)
	       (heading (:name "H1.1" :toc t :level 1))
	       (heading (:name "H1.2" :toc t :level 1)
		(heading (:name "H1.2.1" :toc t :level 2))))
	      (heading (:name "H2" :toc t :level 0)))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))


;;
;; Set Toc
;;

(define-test test-set-toc-1 ()
  (let ((doc '((heading (:name "H1" :toc t))
	       (heading (:name "H2" :toc t))
	       (toc)
	       (heading (:name "H3" :toc t)))))
    (let ((updated-doc (cl-html-readme::set-toc doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t))
	      (heading (:name "H2" :toc t))
	      (toc-root ()
	       (toc-item (:name "H1"))
	       (toc-item (:name "H2"))
	       (toc-item (:name "H3")))
	      (heading (:name "H3" :toc t)))))
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
    (let ((updated-doc (cl-html-readme::set-heading-ids doc))
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
    (let ((updated-doc (cl-html-readme::set-heading-ids doc))
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
    (let ((updated-doc (cl-html-readme::set-heading-ids doc))
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
    (let ((updated-doc (cl-html-readme::set-heading-ids doc))
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
    (let ((updated-doc (cl-html-readme::set-heading-ids doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :id "H1"))
	      (heading (:name "H2"))
	      (heading (:name "H3" :toc t :id "H3")))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))
