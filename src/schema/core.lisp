(defpackage #:apispec/schema/core
  (:use #:cl
        #:apispec/utils
        #:trivial-cltl2)
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
           #:make-schema
           #:defschema

           ;; Don't export because these conflicts with :cl
           ;#:number
           ;#:float
           ;#:integer
           ;#:string
           ;#:byte
           ;#:boolean
           ;#:array

           #:double
           #:binary
           #:date
           #:date-time
           #:email
           #:uuid
           #:object
           #:object-properties
           #:object-additional-properties))
(in-package #:apispec/schema/core)

;; Set safety level 3 for CLOS slot type checking.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *previous-safety*
    (or (assoc 'safety (declaration-information 'optimize))
        '(safety 1)))
  (proclaim '(optimize safety)))

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
  ((type :type cl:string)
   (format :type cl:string)
   (enum :type proper-list
         :initarg :enum)
   (default :type t
            :initarg :default)
   (nullable :type cl:boolean
             :initarg :nullable)
   (deprecated :type cl:boolean
               :initarg :deprecated)))

(defgeneric make-schema (class &rest initargs)
  (:method (class &rest initargs)
    (apply #'make-instance (find-schema class) initargs)))

(defclass number (schema)
  ((type :initform "number")
   (multiple-of :type (cl:real 0)
                :initarg :multiple-of)
   (maximum :type cl:real
            :initarg :maximum)
   (exclusive-maximum :type cl:boolean
                      :initarg :exclusive-maximum)
   (minimum :type cl:real
            :initarg :minimum)
   (exclusive-minimum :type cl:boolean
                      :initarg :exclusive-minimum)))

(defmethod initialize-instance ((object number) &rest initargs
                                &key maximum exclusive-maximum
                                  minimum exclusive-minimum &allow-other-keys)
  ;; :exclusive-{minimum,maximum} can be specified with :{minimum,maximum}
  (declare (ignore initargs))
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
   (max-length :type (cl:integer 0)
               :initarg :max-length)
   (min-length :type (cl:integer 0)
               :initarg :min-length)
   (pattern :type cl:string
            :initarg :pattern)))

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

(defclass boolean (schema)
  ((type :initform "boolean")))

(defclass array (schema)
  ((type :initform "array")
   (items :type schema
          :initarg :items)
   (max-items :type (cl:integer 0)
              :initarg :max-items)
   (min-items :type (cl:integer 0)
              :initarg :min-items)
   (unique-items :type cl:boolean
                 :initarg :unique-items)))

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
  ((name :type (or cl:symbol cl:string)
         :initarg :name)
   (type :type schema
         :initarg :type)))

(defclass object (schema)
  ((type :initform "object")
   (required :type (proper-list (or cl:symbol cl:string))
             :initarg :required)
   (properties :type (proper-list property)
               :initarg :properties
               :initform nil
               :reader object-properties)
   (max-properties :type (cl:integer 0)
                   :initarg :max-properties)
   (min-properties :type (cl:integer 0)
                   :initarg :min-properties)
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
                  :key (lambda (x) (slot-value x 'name))
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

(defmacro schema (schema-definition)
  (multiple-value-bind (type args)
      (parse-schema-definition schema-definition)
    (case type
      (object
       `(make-schema ',type ',(first args) ,@(rest args)))
      (otherwise
       `(make-schema ',type ,@args)))))

(defmacro defschema (name (&optional (superclass 'object)) &body initargs)
  ;; Allow to omit the first ':properties' key.
  (when (consp (first initargs))
    (setf initargs
          (append
           `(:properties (make-properties ',(first initargs)))
           (rest initargs))))
  `(defclass ,name (,superclass)
     ()
     (:default-initargs ,@initargs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim `(optimize ,*previous-safety*)))
