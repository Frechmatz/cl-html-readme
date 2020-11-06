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
  "Test that id attribute is generated for toc marked heading element"
  (let ((dsl `((heading (:name "heading" :toc t)))))
    (let ((str (cl-html-readme-test::dsl-to-string dsl)))
      (assert-true (cl:search "id=" str)))))

