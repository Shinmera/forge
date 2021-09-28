#|
This file is a part of forge
(c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defclass message (component)
  ((name :initarg :name :initform (error "NAME") :reader name)
   (message :initarg :message :initform (error "MESSAGE") :reader message)
   (depends-on :initarg :depends-on :initform () :reader depends-on)))
(defclass print-op (operation)
  ())
(defclass print-effect (effect) ())

(defmethod supported-operations ((message message))
  '(print-op))

(defmethod dependencies ((op print-op) (c message))
  (loop for spec in (depends-on c)
        collect (if (listp spec)
                    (destructuring-bind (parameters version) spec
                      (depend 'print-effect parameters :version (parse-constraint version)))
                    (depend 'print-effect spec))))

(defmethod make-effect ((op print-op) (c message))
  (ensure-effect op c 'print-effect (name c)))

(defmethod perform ((op print-op) (c message) (client client))
  (with-client-eval (client)
    `(write-string ,(message c) *standard-output*)))

(setf *database* (make-instance 'basic-database))
(make-instance 'message :name 0 :message "0")
(make-instance 'message :name 1 :message "A" :depends-on '(0) :version 1)
(make-instance 'message :name 1 :message "B" :depends-on '() :version 2)
(make-instance 'message :name 1 :message "C" :depends-on '(2) :version 3)
(make-instance 'message :name 2 :message "2" :depends-on '(0))
(make-instance 'message :name 3 :message "3" :depends-on '((1 ([ 1 2)) 2))
(make-instance 'message :name 4 :message "4" :depends-on final-dependency)

(defun run-test (final-dependency)
  
  (let ((plan (compute-plan (find-effect *database* 'print-effect 4 (parse-constraint T))
                            (make-instance 'basic-policy)))
        (executor (make-instance 'basic-executor)))
    (execute plan executor)))
