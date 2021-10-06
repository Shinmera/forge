#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defclass parent-component (component)
  ((children :initform (make-hash-table :test 'equal) :reader children)))

(defclass simple-dependent-component (component)
  ((dependencies :initform () :reader dependencies)))

(defmethod shared-initialize :after ((component simple-dependent-component) slots &key (dependencies NIL dependencies-p))
  (when dependencies-p
    (setf (slot-value component 'dependencies) (loop for dependency in dependencies
                                                     collect (normalize-dependency dependency component)))))

(defgeneric normalize-dependency (dependency-spec component))

(defmethod normalize-dependency ((dependency dependency) (component simple-depedent-component))
  (list T dependency))

(defmethod dependencies append ((operation operation) (component simple-dependent-component))
  (loop for (op-type depedency) in (dependencies component)
        when (typep operation op-type)
        collect dependency))

(defclass project (parent-component simple-dependent-component)
  ((name :initarg :name :initform (support:arg! :name) :reader name)
   metadata))

(defmethod make-step ((operation operation) (project project) (effect effect))
  (make-instance 'compound-step
                 :operation operation
                 :component component
                 :effect effect
                 :inner-effect (in-order-to operation project)))

(defgeneric in-order-to (operation project))

(defmethod normalize-dependency ((project project) (component simple-depedent-component)))

(defmacro define-project (modules &body args)
  (mapc #'load-module modules)
  (let ((primary (or (first modules) 'forge)))
    `(register-project ,(parse-project primary args)
                       ,(or *compile-file-truename* *load-truename*))))

(defgeneric load-module (module &key if-exists))
(defgeneric find-module (module &key if-does-not-exist))
(defgeneric parse-project (module project-definition))

(defgeneric find-project (name &key version if-does-not-exist))
(defgeneric register-project (project &optional source-path))
(defgeneric delete-project (project))
