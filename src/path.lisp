(uiop:define-package #:apispec/path
  (:use #:cl
        #:apispec/utils)
  (:use-reexport #:apispec/path/item)
  (:import-from #:assoc-utils
                #:aget))
(in-package #:apispec/path)

(declaim-safety)

(defclass mapper ()
  ((%paths :type paths
           :initform '())))

(defun add-operation (mapper method path-rule operation)
  (check-type path-rule string)
  (let* ((paths (slot-value mapper '%paths))
         (path-item
           (or (aget paths path-rule)
               (let ((new-item (make-instance 'path-item)))
                 (setf (aget paths path-rule)
                       new-item)
                 (setf (slot-value mapper '%paths) paths)
                 new-item))))
    (funcall
      (ecase method
        (:get #'(setf path-item-get))
        (:put #'(setf path-item-put))
        (:post #'(setf path-item-post))
        (:delete #'(setf path-item-delete))
        (:options #'(setf path-item-options))
        (:head #'(setf path-item-head))
        (:patch #'(setf path-item-patch))
        (:trace #'(setf path-item-trace)))
      operation
      path-item)))

(undeclaim-safety)
