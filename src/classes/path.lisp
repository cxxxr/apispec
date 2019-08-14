(defpackage #:apispec/classes/path
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/parameter
                #:parameter)
  (:import-from #:apispec/classes/operation
                #:operation)
  (:export #:path-item
           #:path-item-summary
           #:path-item-description
           #:path-item-parameters
           #:path-item-get
           #:path-item-put
           #:path-item-post
           #:path-item-delete
           #:path-item-options
           #:path-item-head
           #:path-item-patch
           #:path-item-trace
           #:paths))
(in-package #:apispec/classes/path)

(declaim-safety)

(defclass path-item ()
  ((summary :type (or string null)
            :initarg :summary
            :initform nil
            :reader path-item-summary)
   (description :type (or string null)
                :initarg :description
                :initform nil
                :reader path-item-description)
   (parameters :type (proper-list parameter)
               :initarg :parameters
               :initform nil
               :reader path-item-parameters)
   (get :type (or operation null)
        :initarg :get
        :initform nil
        :accessor path-item-get)
   (put :type (or operation null)
        :initarg :put
        :initform nil
        :accessor path-item-put)
   (post :type (or operation null)
         :initarg :post
         :initform nil
         :accessor path-item-post)
   (delete :type (or operation null)
           :initarg :delete
           :initform nil
           :accessor path-item-delete)
   (options :type (or operation null)
            :initarg :options
            :initform nil
            :accessor path-item-options)
   (head :type (or operation null)
         :initarg :head
         :initform nil
         :accessor path-item-head)
   (patch :type (or operation null)
          :initarg :patch
          :initform nil
          :accessor path-item-patch)
   (trace :type (or operation null)
          :initarg :trace
          :initform nil
          :accessor path-item-trace)))

(deftype paths ()
  '(association-list string path-item))

(undeclaim-safety)
