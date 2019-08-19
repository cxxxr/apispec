(defpackage #:apispec/classes/operation
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/parameter
                #:parameter
                #:parameter-in
                #:parse-query-string
                #:parse-path-parameters
                #:parse-headers
                #:parse-cookie-string)
  (:import-from #:apispec/classes/request-body
                #:request-body)
  (:import-from #:apispec/classes/response
                #:responses
                #:encode-response)
  (:export #:operation
           #:operation-tags
           #:operation-summary
           #:operation-description
           #:operation-id
           #:operation-parameters
           #:operation-request-body
           #:operation-responses
           #:operation-deprecated-p
           #:validate-request
           #:validate-response))
(in-package #:apispec/classes/operation)

(declaim-safety)

;; TODO: 'externalDocs', 'callbacks', 'security' and 'servers'.
(defclass operation ()
  ((tags :type (proper-list string)
         :initarg :tags
         :initform nil
         :reader operation-tags)
   (summary :type (or string null)
            :initarg :summary
            :initform nil
            :reader operation-summary)
   (description :type (or string null)
                :initarg :description
                :initform nil
                :reader operation-description)
   (id :type (or string null)
       :initarg :id
       :initform nil
       :reader operation-id)
   (parameters :type (proper-list parameter)
               :initarg :parameters
               :initform nil
               :reader operation-parameters)
   (request-body :type (or request-body null)
                 :initarg :request-body
                 :initform nil
                 :reader operation-request-body)
   (responses :type responses
              :initarg :responses
              :initform (error ":responses is required for OPERATION")
              :reader operation-responses)
   (deprecated :type boolean
               :initarg :deprecated
               :initform nil
               :reader operation-deprecated-p)))

(defun validate-request (operation env &key path-parameters additional-parameters)
  (let ((parameters (append additional-parameters
                            (operation-parameters operation))))
    (loop for parameter in parameters
          for in = (parameter-in parameter)
          if (string= in "path")
          collect parameter into operation-path-parameters
          else if (string= in "query")
          collect parameter into operation-query-parameters
          else if (string= in "header")
          collect parameter into operation-header-parameters
          else if (string= in "cookie")
          collect parameter into operation-cookie-parameters
          finally
          (return
            (append
              (parse-path-parameters
                path-parameters
                operation-path-parameters)
              (parse-query-string
                (getf env :query-string)
                operation-query-parameters)
              (let ((headers (getf env :headers)))
                (append
                  (parse-headers
                    headers
                    operation-header-parameters)
                  (when headers
                    (parse-cookie-string
                      (gethash "cookie" headers)
                      operation-cookie-parameters)))))))))

(defun validate-response (operation status headers data)
  (encode-response status
                   headers
                   data
                   (operation-responses operation)))

(undeclaim-safety)
