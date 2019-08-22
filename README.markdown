# apispec

[![Build Status](https://travis-ci.org/fukamachi/apispec.svg?branch=master)](https://travis-ci.org/fukamachi/apispec)
[![Coverage Status](https://coveralls.io/repos/fukamachi/apispec/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/apispec)

A Common Lisp library for handling Web API specifications. This allows to validate and parse HTTP request headers, parameters and bodies with OpenAPI3 specification.

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Prerequisite

* [libyaml](http://pyyaml.org/wiki/LibYAML) for loading OpenAPI spec files.

## Usage

### Loading specification file

```common-lisp
(defvar *spec*
  (apispec:load-from-file #P"docs/api.yaml"))

(apispec:spec-version *spec*)
;=> "3.0.2"
```

### Getting the operation

```common-lisp
(defvar *router* (apispec:spec-router *spec*))

(apispec:find-route *router* :GET "/products/12")
;=> #<APISPEC/CLASSES/OPERATION:OPERATION {1003DDB073}>
```

### Parsing and Validating HTTP requests

```common-lisp
(import '(lack.request:request-query-parameters
          lack.request:request-body-parameters
          lack.request:request-cookies
          apispec:request-path-parameters))

;; Clack application
(defvar *app*
  (lambda (env)
    (multiple-value-bind (operation path-parameters)
        (apispec:find-route (spec-router *spec*)
                            (getf env :request-method)
                            (getf env :path-info))
      ;; Getting Lack.Request
      (let ((request (apispec:validate-request operation env
                                               :path-parameters path-parameters)))
        ;; Write the main application here.

        ;; Accessors for getting each parameters.
        (request-query-parameters request)  ;=> Query parameters (alist)
        (request-body-parameters request)   ;=> Body parameters (alist)
        (request-path-parameters request)   ;=> Path parameters (alist)
        (request-cookies)                   ;=> Cookie parameters (alist)

        ))))

;; Start the server
(clack:clackup *app*)
```

### Validating and Encoding HTTP responses

```common-lisp
(import 'lack.response:make-response)

(apispec:validate-response operation
                           (make-response 200
                                          '(:content-type "application/json")
                                          '(("id" . 3)
                                            ("name" . "初音ミク")
                                            ("is_hidden" . nil))))
;=> (200 (:CONTENT-TYPE "application/json") ("{\"id\":3,\"name\":\"初音ミク\",\"is_hidden\":false}"))
```

### Custom Encoder for standard objects

```common-lisp
(import 'lack.response:make-response)

;; Custom class
(defclass product ()
  ((id :initarg :id)
   (name :initarg :name)
   (is-hidden :initarg :is-hidden)))

;; Define APISPEC:ENCODE-OBJECT for the class
(defmethod apispec:encode-object ((product product))
  `(("id" . ,(slot-value product 'id))
    ("name" . ,(slot-value product 'name))
    ("is_hidden" . ,(slot-value product 'is-hidden))))

(defvar *yukari*
  (make-instance 'product
                 :id 14
                 :name "結月ゆかり"
                 :is-hidden nil))

(apispec:validate-response operation
                           (make-response 200
                                          '(:content-type "application/json")
                                          *yukari*))
;=> (200 (:CONTENT-TYPE "application/json") ("{\"id\":14,\"name\":\"結月ゆかり\",\"is_hidden\":false}"))
```

## Example

### With ningle

Here's an example of using with [ningle](https://github.com/fukamachi/ningle), a super micro web application framework.

OpenAPI 3 YAML file (docs/api.yaml):

```yaml
openapi: '3.0.1'
info:
  description: >-
    Sample RESTful APIs
  title: Sample RESTful APIs
  version: 0.9.1
paths:
  /products:
    get:
      parameters: []
      responses:
        '200':
          description: OK
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: '#/components/schemas/Product'
  /products/{id}:
    get:
      parameters:
components:
  schemas:
    Product:
      properties:
        id:
          type: integer
        name:
          type: string
        is_hidden:
          type: boolean
```

ningle application with apispec:

```common-lisp
(import '(ningle:*request* ningle*response*))
(import '(lack.component:call lack.response:response-body lack.response:response-headers))

;; Your application class
(defclass web (ningle:app)
  ((spec :initarg :spec)))

;; Main function
(defmethod call ((app web) env)
  (multiple-value-bind (operation path-parameters)
      (apispec:find-route (apispec:spec-router (slot-value app 'spec))
                          (getf env :request-method)
                          (getf env :path-info))
    (if operation
        (let ((*request*
                (apispec:validate-request operation env
                                          :path-parameters path-parameters)))
          (setf (response-body *response*)
                (call-next-method))
          (apispec:validate-response operation *response*))
        (call-next-method))))

;; Creating an instance
(defparameter *app*
  (make-instance 'web
                 :spec (apispec:load-from-file #P"docs/api.yaml")))

;; Defining routes
(setf (ningle:route *app* "/products" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (setf (response-headers *response*)
              '(:content-type "application/json"))
        '((("id" . 3)
           ("name" . "初音ミク")
           ("is_hidden" . nil))
          (("id" . 14)
           ("name" . "結月ゆかり")
           ("is_hidden" . nil)))))

*app*
```

## See Also

* [OpenAPI Specification](https://github.com/OAI/OpenAPI-Specification)
* [Lack](https://github.com/fukamachi/lack)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2019 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 3-Clause License.
