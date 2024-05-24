(in-package :cl-html-readme-plist-util)

;;
;; Property list tooling
;;

(defun with-properties (plist callback-fn)
  "Iterate over properties and call handler with key and value."
  (let ((accept-key t) (key nil))
    (dolist (item plist)
      (if accept-key
	  (setf key item)
	  (progn
	    (funcall callback-fn key item)
	    (setf key nil)))
      (setf accept-key (not accept-key)))))

(defun filter-properties (plist filter-fn)
  "Filter entries of a property list.
   filter-fn: A function that is called with a property keyword. If the function returns nil
   then the property will be omitted from the resulting property list."
  (let ((result nil))
    (with-properties
      plist
      (lambda (key value)
	(if (funcall filter-fn key)
	    (progn
	      (push value result)
	      (push key result)))))
    result))

(defun sort-by-key (plist)
  "Sort a property by its keys. Ascending (A...Z). Keys converted to lowercase symbol-name.
   Order is not defined for multiple occurrences of same key."
  (if (= 2 (length plist))
      plist
      (let ((keys nil))
	;; Collect keys
	(with-properties
	  plist
	  (lambda (key value)
	    (declare (ignore value))
	    (push key keys)))
	;; Sort keys
	(let ((sorted-keys
		(sort
		 keys
		 (lambda (a b)
		   (string-lessp
		    (string-downcase (symbol-name a))
		    (string-downcase (symbol-name b)))))))
	  ;; Create sorted plist
	  (let ((result nil))
	    (dolist (key sorted-keys)
	      (push key result)
	      (push (getf plist key) result))
	    (reverse result))))))
