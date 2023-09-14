(in-package :cl-html-readme-test)

(define-test test-doc-to-string-1 ()
  (let ((doc '((heading (:name "H1" :toc t))
	       (heading (:name "H2" :toc t))
	       (heading (:name "H3" :toc t)))))
    (let* ((doc-str (doc-to-string doc))
	   (expected-doc-str
	     (concatenate
	      'string
	      "((heading (:name 'H1' :toc t)) "
	      "(heading (:name 'H2' :toc t)) "
	      "(heading (:name 'H3' :toc t)))")))
      (assert-equal expected-doc-str doc-str))))

(define-test test-doc-to-string-2 ()
  (let ((doc '((heading (:name "H1" :toc t))
	       (heading (:name "H2" :toc t)
		(heading (:name "H2.1" :toc t))
		(heading (:name "H2.2" :toc t)))
	       (heading (:name "H3" :toc t)))))
    (let* ((doc-str (doc-to-string doc))
	   (expected-doc-str
	     (concatenate
	      'string
	      "((heading (:name 'H1' :toc t)) "
	      "(heading (:name 'H2' :toc t)"
	      " (heading (:name 'H2.1' :toc t))"
	      " (heading (:name 'H2.2' :toc t))) "
	      "(heading (:name 'H3' :toc t)))")))
      (assert-equal expected-doc-str doc-str))))
