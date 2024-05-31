(in-package :cl-html-readme-test)

(define-test test-plist-util-with-properties-1 ()
  (let ((result nil))
    (cl-html-readme-plist-util:with-properties
	(list :a "AA" :b "BB")
      (lambda (key value)
	(push key result)
	(push value result)))
    (assert-equal 4 (length result))
    (setf result (reverse result))
    (assert-equal :a (first result))
    (assert-equal "AA" (second result))
    (assert-equal :b (third result))
    (assert-equal "BB" (fourth result))))

(define-test test-plist-util-filter-properties-1 ()
  (let ((result
	  (cl-html-readme-plist-util:filter-properties
	   (list :a "AA" :b "BB")
	   (lambda (key) (eq key :a)))))
    (assert-equal 2 (length result))
    (assert-equal :a (first result))
    (assert-equal "AA" (second result))))

(define-test test-plist-util-sort-by-key-1 ()
  (let ((sorted
	  (cl-html-readme-plist-util:sort-by-key
	   (list :b "BB" :a "AA"))))
    (assert-equal 4 (length sorted))
    (assert-equal :a (first sorted))
    (assert-equal "AA" (second sorted))
    (assert-equal :b (third sorted))
    (assert-equal "BB" (fourth sorted))))

(define-test test-plist-util-sort-by-key-2 ()
  (let ((sorted
	  (cl-html-readme-plist-util:sort-by-key
	   (list :b "BB"))))
    (assert-equal 2 (length sorted))
    (assert-equal :b (first sorted))
    (assert-equal "BB" (second sorted))))

(define-test test-plist-util-sort-by-key-3 ()
  (let ((plist (list :b "BB")))
    (let ((sorted (cl-html-readme-plist-util:sort-by-key plist)))
	  (assert-true (eq plist sorted)))))


(define-test test-plist-util-sort-by-key-stable ()
  (let ((sorted
	  (cl-html-readme-plist-util:sort-by-key
	   (list :a "a1" :a "a2" :b "b1" :a "a3" :b "b2"))))
    (assert-equal 10 (length sorted))
    ;;
    (assert-equal :a (first sorted))
    (assert-equal "a1" (second sorted))
    ;;
    (assert-equal :a (third sorted))
    (assert-equal "a2" (fourth sorted))
    ;;
    (assert-equal :a (fifth sorted))
    (assert-equal "a3" (sixth sorted))
    ;;
    (assert-equal :b (seventh sorted))
    (assert-equal "b1" (eighth sorted))
    ;;
    (assert-equal :b (ninth sorted))
    (assert-equal "b2" (tenth sorted))))


(define-test test-plist-util-unique-1 ()
  (let ((plist
	  (list :a "a1" :a "a2" :b "b1" :a "a3" :b "b2")))
    (let ((unique (cl-html-readme-plist-util:unique plist)))
      (assert-equal 4 (length unique))
      ;;
      (assert-equal :a (first unique))
      (assert-equal "a1" (second unique))
      ;;
      (assert-equal :b (third unique))
      (assert-equal "b1" (fourth unique)))))

(define-test test-plist-util-unique-2 ()
  (let ((plist
	  (list :a "a1")))
    (let ((unique (cl-html-readme-plist-util:unique plist)))
      (assert-equal 2 (length unique))
      ;;
      (assert-equal :a (first unique))
      (assert-equal "a1" (second unique)))))
