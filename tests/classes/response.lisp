(defpackage #:apispec/tests/classes/response
  (:use #:cl
        #:rove
        #:apispec/classes/response)
  (:import-from #:apispec/classes/media-type
                #:media-type
                #:media-type-schema)
  (:import-from #:apispec/classes/schema
                #:schema
                #:binary
                #:object))
(in-package #:apispec/tests/classes/response)

(deftest find-response-tests
  (let ((responses `((200 . ,(make-instance 'response
                                            :description "Success"
                                            :content
                                            `(("application/json"
                                               . ,(make-instance 'media-type
                                                                 :schema (schema object)))))))))
    (ok (find-response responses 200))
    (ok (signals (find-response responses 404)
                 'response-not-defined))
    (ok (find-response
          (append responses
                  `(("default" . ,(make-instance 'response
                                                 :description "All responses"
                                                 :content
                                                 '(("application/json" . nil))))))
          404))))

(deftest find-media-type-tests
  (let ((response (make-instance 'response
                                 :description "Success"
                                 :content
                                 `(("application/json"
                                    . ,(make-instance 'media-type
                                                      :schema (schema object)))
                                   ("application/x-www-form-urlencoded"
                                    . ,(make-instance 'media-type
                                                      :schema (schema object)))))))
    (ok (find-media-type response "application/json"))
    (ok (find-media-type response "application/json; charset=utf-8"))
    (ok (signals (find-media-type response "application/xml")
                 'response-not-defined)))

  (let ((response (make-instance 'response
                                 :description "All responses"
                                 :content `(("application/octet-stream"
                                             . ,(make-instance 'media-type
                                                               :schema (schema binary)))
                                            ("*/*"
                                             . ,(make-instance 'media-type
                                                               :schema (schema object)))))))
    (let ((media-type (find-media-type response "application/octet-stream")))
      (ok (typep media-type 'media-type))
      (ok (typep (media-type-schema media-type) 'binary)))
    (let ((media-type (find-media-type response "image/png")))
      (ok (typep media-type 'media-type))
      (ok (typep (media-type-schema media-type) 'object)))))

(deftest encode-response-tests
  (ok (equalp (encode-response 200
                               '(("content-type" . "application/json"))
                               '(("id" . 1)
                                 ("is_registered" . nil))
                               `((200 . ,(make-instance 'response
                                                        :description "Success"
                                                        :content
                                                        `(("application/json"
                                                           . ,(make-instance 'media-type
                                                                             :schema (schema
                                                                                       (object
                                                                                         (("id" integer)
                                                                                          ("is_registered" boolean)))))))))))
              '(200
                (:content-type "application/json")
                ("{\"id\":1,\"is_registered\":false}")))))
