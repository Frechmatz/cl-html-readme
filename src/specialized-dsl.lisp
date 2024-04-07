(in-package :cl-html-readme-specialized-dsl)

(defclass specialized-dsl ()
  ((special-forms
    :initform (list)))
  (:documentation
   "Based on the Low-Level DSL. Defines actually supported special forms and their properties.
    Properties are represented by plists."))

(defgeneric register-special-form (specialized-dsl form-symbol mandatory-form-properties optional-form-properties)
  (:documentation
   "Register a special form. Properties are represented by plists.
    <ul>
    <li>form-symbol A symbol, for example <code>'heading</code>. The symbol can belong to any package.</li>
    <li>mandatory-form-properties A list of keywords defining the mandatory properties of the form.</li>
    <li>optional-form-properties A list of keywords defining the optional properties of the form.</li>
    </ul></p>"))

(defgeneric is-special-form (specialized-dsl form-symbol expected-form-symbol)
  (:documentation
   "Returns t if a given form-symbol matches a certain form-symbol exposed by the DSL.
    <p>Example: <code>(is-special-form instance form-symbol 'heading)</code></p>"))

(defgeneric validate-special-form (specialized-dsl form-symbol form-properties)
  (:documentation "Validate the properties of a given special-form.
   <p>Applies the following checks
   <ul>
   <li>That the form-symbol represents a special form exposed by the DSL.</li>
   <li>That all mandatory properties of the special form are defined.</li>
   <li>That the form properties do not define properties outside of mandatory and optional ones.</li>
   </ul></p>"))

(defun get-special-form-definition (instance form-symbol)
  "Internal symbol lookup function. Does not assume any package into which symbols are interned and therefore
   goes with comparison of symbol names."
  (if (not (symbolp form-symbol))
      nil
      (let ((name (symbol-name form-symbol)))
	(find-if (lambda(e) (string= name (getf e :name))) (slot-value instance 'special-forms)))))

;;
;; Implementation
;;

(defmethod register-special-form ((instance specialized-dsl) form-symbol mandatory-properties optional-properties)
  (with-slots (special-forms) instance
    (let ((form
	    (list
	     :name (symbol-name form-symbol) ;; do not assume interred package. Go with name
	     :mandatory-properties mandatory-properties
	     :optional-properties optional-properties)))
      (push form special-forms))))

(defmethod is-special-form ((instance specialized-dsl) form-symbol expected-form-symbol)
  (let ((form-definition (get-special-form-definition instance expected-form-symbol)))
    (if (not form-definition)
	nil
	(string= (symbol-name form-symbol) (getf form-definition :name)))))

(defun signal-syntax-error (format-control format-arguments)
  (apply #'format t (concatenate 'string "~%" format-control "~%") format-arguments)
  (error
   'cl-html-readme:syntax-error
   :format-control format-control
   :format-arguments format-arguments))

(defmethod validate-special-form((instance specialized-dsl) form-symbol form-properties)
  (let ((form-definition (get-special-form-definition instance form-symbol)))
    (if (not form-definition)
	(signal-syntax-error "Undefined DSL form '~a'" (list form-symbol)))
    (dolist (key (getf form-definition :mandatory-properties))
      (if (not (getf form-properties key))
	  (signal-syntax-error "Mandatory property ~a missing in form ~a" (list key form-symbol))))
    nil))


