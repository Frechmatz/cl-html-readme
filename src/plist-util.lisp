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
  "Stable sort of a property list by its keys. Ascending (A...Z).
   Key comparison via lowercase symbol-name."
  (if (= 2 (length plist))
      plist
      ;; Create intermediate representation
      (let ((intermediate nil))
	(with-properties
	  plist
	  (lambda (key value)
	    (push (list :key key :value value) intermediate)))
	;; Sort
	(let ((sorted
		(stable-sort
		 (reverse intermediate)
		 (lambda (a b)
		   (string-lessp
		    (string-downcase (symbol-name (getf a :key)))
		    (string-downcase (symbol-name (getf b :key))))))))
	  ;; Convert intermediate representation back to plist
	  (let ((result nil))
	    (dolist (entry sorted)
	      (push (getf entry :key) result)
	      (push (getf entry :value) result))
	    (reverse result))))))
