(defpackage #:apispec/parameter/core
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/schema
                #:schema)
  (:export #:parameter
           #:parameter-name
           #:parameter-in
           #:parameter-required-p
           #:parameter-schema
           #:parameter-style
           #:parameter-explode-p
           #:parameter-allow-reserved-p))
(in-package #:apispec/parameter/core)

(declaim-safety)

(defun parameter-in-string-p (in)
  (and (member in '("path" "query" "header" "cookie")
               :test #'equal)
       t))

(deftype parameter-in ()
  '(satisfies parameter-in-string-p))

(defun parameter-style-string-p (style)
  (and (member style '("matrix" "label" "form" "simple" "spaceDelimited" "pipeDelimited" "deepObject")
               :test #'equal)
       t))

(deftype parameter-style ()
  '(satisfies parameter-style-string-p))

(defclass parameter ()
  ((name :type string
         :initarg :name
         :initform (error ":name is required for PARAMETER")
         :reader parameter-name)
   (in :type parameter-in
       :initarg :in
       :initform (error ":in is required for PARAMETER")
       :reader parameter-in)
   (required :type boolean
             :initarg :required
             :initform nil
             :reader parameter-required-p)
   (schema :type (or schema null)
           :initarg :schema
           :initform nil
           :reader parameter-schema)
   (style :type parameter-style
          :initarg :style)
   (explode :type boolean
            :initarg :explode)
   (allow-reserved :type boolean
                   :initarg :allow-reserved
                   :initform nil
                   :reader parameter-allow-reserved-p)))

(defmethod initialize-instance ((object parameter) &rest initargs
                                &key in (required nil required-supplied-p) default style
                                &allow-other-keys)
  (when (equal in "path")
    (when (and required-supplied-p
               (not required))
      (error ":required must be 'true' for 'path' parameters."))
    (setf (getf initargs :required) t
          required t))

  (when style
    (cond
      ((equal style "matrix")
       (assert (equal in "path")))
      ((equal style "label")
       (assert (equal in "path")))
      ((equal style "form")
       (assert (or (equal in "query")
                   (equal in "cookie"))))
      ((equal style "simple")
       (assert (or (equal in "path")
                   (equal in "header"))))
      ((or (equal style "spaceDelimited")
           (equal style "pipeDelimited")
           (equal style "deepObject"))
       (assert (equal in "query")))))

  (when (and default required)
    (error ":default cannot be specified for required parameters"))

  (apply #'call-next-method object initargs))

(defun parameter-style (parameter)
  (check-type parameter parameter)
  (if (slot-boundp parameter 'style)
      (slot-value parameter 'style)
      (with-slots (in) parameter
        (cond
          ((or (equal in "query")
               (equal in "cookie")) "form")
          ((or (equal in "path")
               (equal in "header")) "simple")))))

(defun parameter-explode-p (parameter)
  (check-type parameter parameter)
  (if (slot-boundp parameter 'explode)
      (slot-value parameter 'explode)
      (let ((style (parameter-style parameter)))
        (if (equal style "form")
            t
            nil))))

(undeclaim-safety)
