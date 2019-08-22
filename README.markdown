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

(apispec:find-route router :GET "/products/12")
;=> #<APISPEC/CLASSES/OPERATION:OPERATION {1003DDB073}>
```

### Parsing and Validating HTTP requests

```common-lisp
(import '(lack.request:request-query-parameters
          lack.request:request-body-parameters
          lack.request:request-cookies
          apispec:request-path-parameters))

(defvar *app*
  (lambda (env)
    (multiple-value-bind (operation path-parameters)
        (apispec:find-route (spec-router *spec*)
                            (getf env :request-method)
                            (getf env :path-info))
      ;; Getting Lack.Request
      (let ((request (apispec:validate-request operation env
                                               :path-parameters path-parameters)))
        (request-query-parameters request)  ;=> Query parameters (alist)
        (request-body-parameters request)   ;=> Body parameters (alist)
        (request-path-parameters request)   ;=> Path parameters (alist)
        (request-cookies)                   ;=> Cookie parameters (alist)

        ;; main application
        ))))

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
(defclass vocaloid ()
  ((id :initarg :id)
   (name :initarg :name)
   (is-hidden :initarg :is-hidden)))

(defmethod apispec:encode-object ((vocaloid vocaloid))
  `(("id" . ,(slot-value vocaloid 'id))
    ("name" . ,(slot-value vocaloid 'name))
    ("is_hidden" . ,(slot-value vocaloid 'is-hidden))))

(defvar *yukari*
  (make-instance 'vocaloid
                 :id 14
                 :name "結月ゆかり"
                 :is-hidden nil))

(apispec:validate-response operation
                           (make-response 200
                                          '(:content-type "application/json")
                                          *yukari*))
;=> (200 (:CONTENT-TYPE "application/json") ("{\"id\":14,\"name\":\"結月ゆかり\",\"is_hidden\":false}"))
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
