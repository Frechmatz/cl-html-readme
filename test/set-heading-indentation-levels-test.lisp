(in-package :cl-html-readme-test)


;;
;; Set indentation levels
;;

(define-test test-set-heading-indentation-levels-1 ()
  (let ((doc '((heading (:name "H1" :toc t))
	       (heading (:name "H2" :toc t))
	       (heading (:name "H3" :toc t)))))
    (let ((updated-doc (cl-html-readme-dsl-frontend::set-heading-indentation-levels doc))
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
    (let ((updated-doc (cl-html-readme-dsl-frontend::set-heading-indentation-levels doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :level 0)
	       (heading (:name "H1.1" :toc t :level 1))
	       (heading (:name "H1.2" :toc t :level 1)
		(heading (:name "H1.2.1" :toc t :level 2))))
	      (heading (:name "H2" :toc t :level 0)))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))

