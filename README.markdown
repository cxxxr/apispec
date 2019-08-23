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

(apispec:find-route *router* :GET "/users/12")
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
                                            ("is_admin" . nil))))
;=> (200 (:CONTENT-TYPE "application/json") ("{\"id\":3,\"name\":\"初音ミク\",\"is_admin\":false}"))
```

### Custom Encoder for standard objects

```common-lisp
(import 'lack.response:make-response)

;; Custom class
(defclass user ()
  ((id :initarg :id)
   (name :initarg :name)
   (is-admin :initarg :is-admin)))

;; Define APISPEC:ENCODE-OBJECT for the class
(defmethod apispec:encode-object ((user user))
  `(("id" . ,(slot-value user 'id))
    ("name" . ,(slot-value user 'name))
    ("is_admin" . ,(slot-value user 'is-admin))))

(defvar *yukari*
  (make-instance 'user
                 :id 14
                 :name "結月ゆかり"
                 :is-admin nil))

(apispec:validate-response operation
                           (make-response 200
                                          '(:content-type "application/json")
                                          *yukari*))
;=> (200 (:CONTENT-TYPE "application/json") ("{\"id\":14,\"name\":\"結月ゆかり\",\"is_admin\":false}"))
```

## Examples

See [examples/](examples/).

## See Also

* [OpenAPI Specification](https://github.com/OAI/OpenAPI-Specification)
* [Lack](https://github.com/fukamachi/lack)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2019 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 3-Clause License.
