(in-package :cl-html-readme-dsl-util)

(defclass specialized-dsl ()
  ((special-forms
    :initform (list)))
  (:documentation
   "Based on the Low-Level DSL. Defines actually supported special forms and their properties."))

(defgeneric signal-syntax-error (specialized-dsl format-control format-arguments)
  (:documentation ""))

(defun get-special-form-definition (specialized-dsl-instance form-symbol)
  "Internal symbol lookup function."
  (if (not (symbolp form-symbol))
      nil
      (let ((name (symbol-name form-symbol)))
	(find-if
	 (lambda(e)
	   ;; Compare by name. Do not assume any package.
	   (string= name (getf e :name)))
	 (slot-value specialized-dsl-instance 'special-forms)))))

(defun register-special-form (specialized-dsl
			      form-symbol
			      mandatory-properties
			      optional-properties)
  "Register a special form. Properties are represented by plists.
    <ul>
    <li>form-symbol A symbol, for example <code>'heading</code>. The symbol can belong to any package.</li>
    <li>mandatory-form-properties A list of keywords defining the mandatory properties of the form.</li>
    <li>optional-form-properties A list of keywords defining the optional properties of the form.</li>
    </ul></p>"
  (let ((special-forms (slot-value specialized-dsl 'special-forms)))
    (let ((form
	    (list
	     :name (symbol-name form-symbol) ;; do not assume interred package. Go with name
	     :mandatory-properties mandatory-properties
	     :optional-properties optional-properties
	     :all-allowed-properties (concatenate 'list mandatory-properties optional-properties))))
      (setf (slot-value specialized-dsl 'special-forms) (push form special-forms)))))

(defun is-supported-special-form-property
    (specialized-dsl form-symbol property-keyword)
  "Returns t if given keyword is a supported property of the special form represented
   by form-symbol."
  (let ((form-definition (get-special-form-definition
			  specialized-dsl
			  form-symbol)))
    (find property-keyword (getf form-definition :all-allowed-properties))))

(defun validate-special-form (specialized-dsl
			      form-symbol
			      form-properties)
  "Validate the properties of a given special-form.
   <p>Applies the following checks
   <ul>
   <li>That the form-symbol represents a special form exposed by the DSL.</li>
   <li>That all mandatory properties of the special form are defined.</li>
   <li>That the form properties do not define properties outside of mandatory and optional ones.</li>
   </ul></p>"
  (let ((form-definition (get-special-form-definition specialized-dsl form-symbol)))
    (if (not form-definition)
	(signal-syntax-error
	 specialized-dsl
	 "~a Undefined DSL form '~a'"
	 (list (class-of specialized-dsl) form-symbol)))
    (dolist (key (getf form-definition :mandatory-properties))
      (if (not (getf form-properties key))
	  (signal-syntax-error
	   specialized-dsl
	   "~a Mandatory property '~a' missing in form '~a'"
	   (list (class-of specialized-dsl) key form-symbol))))
    (dolist (key (cl-html-readme-plist-util:get-property-list-keys form-properties))
      (if (not (find key (getf form-definition :all-allowed-properties)))
	  (signal-syntax-error
	   specialized-dsl
	   "~a Property '~a' not supported in form '~a'"
	   (list (class-of specialized-dsl) key form-symbol))))
    nil))
  
