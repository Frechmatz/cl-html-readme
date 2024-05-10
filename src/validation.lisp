(in-package :cl-html-readme-validation)

;;
;;
;;

(defclass validation-util ()
  ()
  (:documentation "Validation utility"))

(defgeneric reject (validation-util format-control format-arguments)
  (:documentation "Error handler"))

;;
;;
;;

(defclass validator ()
  ((name
    :initarg :name
    :documentation "Name of the validator"))
  (:documentation ""))

(defgeneric validate (validator validation-util object)
  (:documentation "Validate properties. The function has the following parameters:
    <ul>
    <li>validation-util Error handler. An instance of <code>validation-util</code></li>
    <li>object An object to be validated.</li></ul>"))

;;
;;
;;

(defclass property-list-validator (validator)
  ((properties
    :initarg :properties
    :documentation "list (:indicator :mandatory)")
   (mandatory
    :initform nil
    :documentation "List of keywords")
   (optional
    :initform nil
    :documentation "List of keywords")  
   (all
    :initform nil
    :documentation "List of keywords"))  
  (:documentation "Property validator"))

(defmethod initialize-instance :after ((instance property-list-validator) &key)
  (with-slots (properties mandatory optional all) instance
    (setf
     mandatory
     (mapcar
      (lambda (p)
	(getf p :indicator))
      (remove-if (lambda (p) (not (getf p :mandatory))) properties)))
    (setf
     optional
     (mapcar
      (lambda (p) (getf p :indicator))
      (remove-if (lambda (p) (getf p :mandatory)) properties)))
    (setf
     all
     (mapcar
      (lambda (p)
	(getf p :indicator))
      properties))))

(defmethod validate ((instance property-list-validator) validation-util property-list)
  (with-slots (mandatory optional all name) instance
    (dolist (key mandatory)
      (if (not (getf property-list key))
	  (reject
	   validation-util 
	   "~a '~a' Mandatory property '~a' missing in form-properties:~%'~a'"
	   (list (class-of instance) name key property-list))))
    (cl-html-readme-plist-util:with-properties
      property-list
      (lambda (key value)
	(declare (ignore value))
      (if (not (find key all))
	  (reject
	   validation-util
	   "~a '~a' Property '~a' not supported in form-properties: ~%'~a'"
	   (list (class-of instance) name key property-list))))))
  nil)


