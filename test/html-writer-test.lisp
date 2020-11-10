(in-package :cl-html-readme-test)

(define-test html-writer-test-1 ()
  (let ((dsl `((heading (:name "heading")))))
    (let ((str (cl-html-readme-test::dsl-to-string dsl)))
      (assert-equal "<h1>heading</h1>" str))))

(define-test html-writer-test-2 ()
  (let ((dsl `((heading (:name "heading" :class "Class" :style "Style")))))
    (let ((str (cl-html-readme-test::dsl-to-string dsl)))
      (assert-equal "<h1 class=\"Class\" style=\"Style\">heading</h1>" str))))

(define-test html-writer-test-3 ()
  (let ((dsl `((heading (:name "heading" :id "ID")))))
    (let ((str (cl-html-readme-test::dsl-to-string dsl)))
      (assert-equal "<h1 id=\"ID\">heading</h1>" str))))
  
(define-test html-writer-test-4 ()
  "Test id generation"
  (let ((dsl `((heading (:name "heading" :toc t)))))
    (let ((str (cl-html-readme-test::dsl-to-string dsl)))
      (assert-equal "<h1 id=\"heading\">heading</h1>" str))))

(define-test html-writer-test-5 ()
  "Test id generation"
  (let ((dsl `((heading (:name "heading" :toc t))(heading (:name "heading" :toc t)))))
    (let ((str (cl-html-readme-test::dsl-to-string dsl)))
      (assert-equal "<h1 id=\"heading\">heading</h1><h1 id=\"heading-1\">heading</h1>" str))))

(define-test html-writer-test-6 ()
  "Test id generation"
  (let ((dsl `((heading (:name "heading" :toc t))(heading (:name "heading2" :toc t))(heading (:name "heading" :toc t)))))
    (let ((str (cl-html-readme-test::dsl-to-string dsl)))
      (assert-equal "<h1 id=\"heading\">heading</h1><h1 id=\"heading2\">heading2</h1><h1 id=\"heading-1\">heading</h1>" str))))
