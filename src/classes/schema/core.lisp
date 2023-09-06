(defpackage #:apispec/classes/schema/core
  (:use #:cl
        #:apispec/utils)
  (:shadow #:number
           #:float
           #:integer
           #:string
           #:byte
           #:boolean
           #:array)
  (:import-from #:trivia
                #:match
                #:ematch
                #:guard)
  (:import-from #:alexandria
                #:ensure-cons)
  (:export #:schema
           #:schemap
           #:schema-type
           #:schema-format
           #:schema-enum
           #:schema-default
           #:schema-has-default-p
           #:schema-nullable-p
           #:schema-deprecated-p
           #:make-schema
           #:defschema

           #:number
           #:number-multiple-of
           #:number-maximum
           #:number-exclusive-maximum-p
           #:number-minimum
           #:number-exclusive-minimum-p
           #:float
           #:double
           #:integer

           #:string
           #:string-max-length
           #:string-min-length
           #:string-pattern
           #:byte
           #:binary
           #:date
           #:date-time
           #:email
           #:uuid
           #:json

           #:boolean

           #:array
           #:array-items
           #:array-max-items
           #:array-min-items
           #:array-unique-items-p

           #:object
           #:object-required
           #:object-properties
           #:object-max-properties
           #:object-min-properties
           #:object-additional-properties
           #:property
           #:property-name
           #:property-type
           #:find-object-property))
(in-package #:apispec/classes/schema/core)

(declaim-safety)

(defun find-schema (schema-class-name)
  (check-type schema-class-name (and symbol (not keyword) (not null)))
  #.`(case schema-class-name
       ,@(loop for cl-class-name in '(cl:number
                                      cl:float
                                      cl-user::double
                                      cl:integer
                                      cl:string
                                      cl:byte
                                      cl:boolean
                                      cl:array)
               collect `(,cl-class-name ',(intern (cl:string cl-class-name)
                                                  *package*)))
       (otherwise schema-class-name)))

(defclass schema ()
  ((type :type (or cl:string (eql t))
         :initform t
         :reader schema-type)
   (format :type (or cl:string null)
           :initform nil
           :reader schema-format)
   (enum :type proper-list
         :initarg :enum
         :initform nil
         :reader schema-enum)
   (default :type t
            :initarg :default)
   (nullable :type cl:boolean
             :initarg :nullable
             :initform nil
             :reader schema-nullable-p)
   (deprecated :type cl:boolean
               :initarg :deprecated
               :initform nil
               :reader schema-deprecated-p)))

(defgeneric make-schema (class &rest initargs)
  (:method (class &rest initargs)
    (apply #'make-instance (find-schema class) initargs)))

(defun schemap (object)
  (typep object 'schema))

(defun schema-has-default-p (schema)
  (slot-boundp schema 'default))

(defun schema-default (schema)
  (assert (schema-has-default-p schema))
  (slot-value schema 'default))

(defclass number (schema)
  ((type :initform "number")
   (multiple-of :type (or (cl:real 0) null)
                :initarg :multiple-of
                :initform nil
                :reader number-multiple-of)
   (maximum :type (or cl:real null)
            :initarg :maximum
            :initform nil
            :reader number-maximum)
   (exclusive-maximum :type cl:boolean
                      :initarg :exclusive-maximum
                      :initform nil
                      :reader number-exclusive-maximum-p)
   (minimum :type (or cl:real null)
            :initarg :minimum
            :initform nil
            :reader number-minimum)
   (exclusive-minimum :type cl:boolean
                      :initarg :exclusive-minimum
                      :initform nil
                      :reader number-exclusive-minimum-p)))

(defmethod initialize-instance ((object number) &rest initargs
                                &key maximum exclusive-maximum
                                  minimum exclusive-minimum &allow-other-keys)
  (declare (ignore initargs))
  ;; :exclusive-{minimum,maximum} can be specified with :{minimum,maximum}
  (assert (or (null exclusive-maximum)
              maximum))
  (assert (or (null exclusive-minimum)
              minimum))

  (call-next-method))

(defun make-number-schema (class &rest initargs)
  (apply #'make-instance (find-schema class)
         (match initargs
           ((list* (guard minimum (numberp minimum))
                   (guard maximum (numberp maximum))
                   options)
            `(:minimum ,minimum :maximum ,maximum ,@options))
           ((list* (guard minimum (numberp minimum))
                   options)
            `(:minimum ,minimum ,@options))
           (otherwise initargs))))

#.`(progn
     ,@(loop for type in '(number cl:number
                           float cl:float
                           double cl-user::double
                           integer cl:integer)
             collect `(defmethod make-schema ((class (eql ',type)) &rest initargs)
                        (apply #'make-number-schema class initargs))))

(defclass float (number)
  ((format :initform "float")))

(defclass double (number)
  ((format :initform "double")))

(defclass integer (number)
  ((type :initform "integer")))

(defclass string (schema)
  ((type :initform "string")
   (max-length :type (or (cl:integer 0) null)
               :initarg :max-length
               :initform nil
               :reader string-max-length)
   (min-length :type (or (cl:integer 0) null)
               :initarg :min-length
               :initform nil
               :reader string-min-length)
   (pattern :type (or cl:string null)
            :initarg :pattern
            :initform nil
            :reader string-pattern)
   (format :initarg :format)))

(defmethod initialize-instance ((object string) &rest initargs
                                &key max-length min-length &allow-other-keys)
  (declare (ignore initargs))
  (when (and min-length max-length)
    (assert (<= min-length max-length)))
  (call-next-method))

(defun make-string-schema (class &rest initargs)
  (apply #'make-instance (find-schema class)
         (match initargs
           ((list* (guard min-length (numberp min-length))
                   (guard max-length (numberp max-length))
                   options)
            `(:min-length ,min-length :max-length ,max-length ,@options))
           ((list* (guard min-length (numberp min-length))
                   options)
            `(:min-length ,min-length ,@options))
           (otherwise initargs))))

#.`(progn
     ,@(loop for type in '(string cl:string
                           byte cl:byte
                           binary
                           email)
             collect `(defmethod make-schema ((class (eql ',type)) &rest initargs)
                        (apply #'make-string-schema class initargs))))

(defclass byte (string)
  ((format :initform "byte")))

(defclass binary (string)
  ((format :initform "binary")))

(defclass date (string)
  ((format :initform "date")))

(defclass date-time (string)
  ((format :initform "date-time")))

(defclass email (string)
  ((format :initform "email")))

(defclass uuid (string)
  ((format :initform "uuid")))

(defclass json (string)
  ((format :initform "json")))

(defclass boolean (schema)
  ((type :initform "boolean")))

(defclass array (schema)
  ((type :initform "array")
   (items :type (or schema null)
          :initarg :items
          :initform nil
          :reader array-items)
   (max-items :type (or (cl:integer 0) null)
              :initarg :max-items
              :initform nil
              :reader array-max-items)
   (min-items :type (or (cl:integer 0) null)
              :initarg :min-items
              :initform nil
              :reader array-min-items)
   (unique-items :type cl:boolean
                 :initarg :unique-items
                 :initform nil
                 :reader array-unique-items-p)))

(declaim (ftype (function (t) t) parse-schema-definition))

(defmethod initialize-instance ((object array) &rest initargs
                                &key items max-items min-items &allow-other-keys)
  (when (and min-items max-items)
    (assert (<= min-items max-items)))
  (unless (or (null items)
              (typep items 'schema))
    (setf (getf initargs :items)
          (multiple-value-bind (type args)
              (parse-schema-definition items)
            (apply #'make-schema type args))))
  (apply #'call-next-method object initargs))

(defun make-array-schema (class &rest initargs)
  (apply #'make-instance (find-schema class)
         (match initargs
           ((list* (guard min-items (numberp min-items))
                   (guard max-items (numberp max-items))
                   options)
            `(:min-items ,min-items :max-items ,max-items ,@options))
           ((list* (guard min-items (numberp min-items))
                   options)
            `(:min-items ,min-items ,@options))
           (otherwise initargs))))

#.`(progn
     ,@(loop for type in '(array cl:array)
             collect `(defmethod make-schema ((class (eql ',type)) &rest initargs)
                        (apply #'make-array-schema class initargs))))

(defclass property ()
  ((name :type cl:string
         :initarg :name
         :initform (error ":name is required for PROPERTY")
         :reader property-name)
   (type :type schema
         :initarg :type
         :initform (error ":type is required for PROPERTY")
         :reader property-type)))

(defclass object (schema)
  ((type :initform "object")
   (required :type (proper-list cl:string)
             :initarg :required
             :initform nil
             :reader object-required)
   (properties :type (proper-list property)
               :initarg :properties
               :initform nil
               :reader object-properties)
   (max-properties :type (or (cl:integer 0) null)
                   :initarg :max-properties
                   :initform nil
                   :reader object-max-properties)
   (min-properties :type (or (cl:integer 0) null)
                   :initarg :min-properties
                   :initform nil
                   :reader object-min-properties)
   (additional-properties :type (or cl:boolean schema)
                          :initarg :additional-properties
                          :initform t
                          :reader object-additional-properties)))

(defmethod initialize-instance ((object object) &rest initargs
                                &key min-properties max-properties
                                  required properties additional-properties
                                &allow-other-keys)
  (when (and min-properties max-properties)
    (assert (<= min-properties max-properties)))

  (dolist (prop-name required)
    (unless (find prop-name properties
                  :key #'property-name
                  :test #'equal)
      (error "Unknown property ~S in :required" prop-name)))

  (unless (or (typep additional-properties 'cl:boolean)
              (typep additional-properties 'schema))
    (setf (getf initargs :additional-properties)
          (multiple-value-bind (type args)
              (parse-schema-definition additional-properties)
            (apply #'make-schema type args))))

  (apply #'call-next-method object initargs))

(defun make-properties (properties-definition)
  (mapcar (lambda (property-definition)
            (ematch property-definition
              ((list* property-name
                      (or (guard property-type (symbolp property-type))
                          (list* (guard property-type (symbolp property-type))
                                 property-args))
                      args)
               (apply #'make-instance 'property
                      :name property-name
                      :type
                      (if property-args
                          (apply #'make-schema property-type (first property-args) (rest property-args))
                          (make-instance (find-schema property-type)))
                      args))))
          properties-definition))

(defun make-object-schema (class properties-definition &rest options)
  (apply #'make-instance (find-schema class)
         :properties (make-properties properties-definition)
         options))

(defmethod make-schema ((class (eql 'object)) &rest initargs)
  (if initargs
      (ematch initargs
        ((list* (list* fields) options)
         (apply #'make-object-schema class fields options)))
      (make-instance (find-schema class))))

(defun expand-nullable (type-specifier)
  (match type-specifier
    ((or (list 'or 'null type)
         (list 'or type 'null))
     (values (ensure-cons type)
             t))
    (otherwise
     (values (ensure-cons type-specifier)
             nil))))

(defun parse-schema-definition (schema-definition)
  (multiple-value-bind (schema-definition is-nullable)
      (expand-nullable schema-definition)
    (destructuring-bind (type &rest args)
        schema-definition
      (case type
        (object
         (destructuring-bind (fields &rest options)
             (ensure-cons args)
           (values type
                   (cons fields
                         (append (and is-nullable
                                      '(:nullable t))
                                 options)))))
        (otherwise
         (values type
                 (append args
                         (and is-nullable
                              '(:nullable t)))))))))

(defun expand-property-definition (property-definitions)
  (mapcar (lambda (prop-def)
            (destructuring-bind (name schema &rest initargs)
                prop-def
              `(make-instance 'property
                              :name ',name
                              :type (schema ,schema)
                              ,@initargs)))
          property-definitions))

(defmacro schema (schema-definition)
  (multiple-value-bind (type args)
      (parse-schema-definition schema-definition)
    (case type
      (object
       `(make-instance (find-schema ',type)
                       :properties (list ,@(expand-property-definition (first args)))
                       ,@(rest args)))
      (otherwise
       `(make-schema ',type ,@args)))))

(defmacro defschema (name (&optional (superclass 'object)) &body initargs)
  ;; Allow to omit the first ':properties' key.
  (when (consp (first initargs))
    (setf initargs
          (append
           `(:properties (list ,@(expand-property-definition (first initargs))))
           (rest initargs))))
  `(defclass ,name (,superclass)
     ()
     (:default-initargs ,@initargs)))

(defun find-object-property (object property-name)
  (check-type object object)
  (check-type property-name cl:string)
  (find property-name (object-properties object)
        :key #'property-name
        :test #'string=))

(undeclaim-safety)
