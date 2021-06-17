(defpackage #:apispec/utils/path-template
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:import-from #:alexandria
                #:when-let)
  (:export #:compile-path-template))
(in-package #:apispec/utils/path-template)

(defun compile-path-template (template &optional (result-value t))
  (check-type template string)
  (let* ((match-vars (ppcre:all-matches-as-strings "(?<={)[^}]+(?=})" template))
         (template-regexp (format nil "^~A$" (ppcre:regex-replace-all "{[^}]+}" template "([^/]+)")))
         (scanner (ppcre:create-scanner template-regexp)))
    (values
      (lambda (path-info)
        (declare (string path-info))
        (when-let (matches (nth-value 1 (ppcre:scan-to-strings scanner path-info)))
          (values result-value
                  (if match-vars
                      (loop for var in match-vars
                            for match across matches
                            collect (cons var match))
                      nil))))
      (length match-vars)
      template-regexp)))
