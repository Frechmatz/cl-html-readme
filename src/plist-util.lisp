(in-package :cl-html-readme-plist-util)

;;
;; Property list tooling
;;

(defun get-property-list-keys (plist)
  "Get the keys of a property list"
  (let ((keys nil) (push-key t))
    (dolist (item plist)
      (if push-key
	  (push item keys))
      (setf push-key (not push-key)))
    keys))

(defun filter-property-list-entries (plist filter-fn)
  "Filter entries of a property list.
   filter-fn: A function that is called with a property keyword. If the function returns nil the property will be omitted from the resulting property list."
  (let ((keys (get-property-list-keys plist))
	(result nil))
    (dolist (key keys)
      (if (funcall filter-fn key)
	  (progn
	    (push (getf plist key) result)
	    (push key result))))
    result))

;; Yes, this check is expensive...for now lets go with it...
(defun has-property (plist keyword)
  "Returns t if the propery list contains a property defined by keyword."
  (find keyword (get-property-list-keys plist)))
