# apispec

A Common Lisp library for handling Web API specifications. This allows to validate and parse HTTP request headers, parameters and bodies by OpenAPI3 specification.

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

### Finding the operation for the request

```common-lisp
(defvar *router* (apispec:spec-router *spec*))

(apispec:find-route router :GET "/pets/12")
;=> #<APISPEC/CLASSES/OPERATION:OPERATION {1003DDB073}>
```

### Validating HTTP requests

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
