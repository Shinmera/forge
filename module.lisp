#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

;;;; Description:
;;; In this file the "module" protocol is defined. Modules allow for extensions
;;; to Forge's capabilities and are what's used to define the required
;;; components, operations, and effects to actually build projects for a
;;; particular language.
;;; The blueprint system hooks into the module system to automatically load the
;;; desired modules into the Forge process.

(in-package #:org.shirakumo.forge)

(support:define-condition* no-such-module (error)
  (designator) ("A module with the name~%  ~s~%does not exist." designator))

(support:define-condition* module-already-exists (error)
  (designator) ("A module with the name~%  ~s~%already exists." designator))

(defvar *module-entry-point-search-functions* (make-hash-table :test 'eql))

(defun module-entry-point-search-function (name)
  (gethash name *module-entry-point-search-functions*))

(defun (setf module-entry-point-search-function) (value name)
  (cond (value
         (setf (gethash name *module-entry-point-search-functions*) value))
        (T
         (remhash name *module-entry-point-search-functions*)
         name)))

(defmacro define-module-entry-point-search-function (name (designator) &body body)
  `(setf (gethash ',name *module-entry-point-search-functions*)
         (lambda (,designator)
           (block NIL
             ,@body))))

(defun find-module-entry-point (designator &key (if-does-not-exist :error))
  (or (loop for function being the hash-values of *module-entry-point-search-functions*
            for value = (funcall function designator)
            when value
            do (return value))
      (ecase if-does-not-exist
        ((NIL) NIL)
        (:error
         (restart-case (error 'no-such-module :designator designator)
           (use-value (value)
             :report "Use the provided value"
             value))))))

(defvar *modules* (make-hash-table :test 'equal))

(defclass module ()
  ((name :initarg :name :initform (support:arg! :name) :reader name)))

(defgeneric load-module (designator &key if-exists if-does-not-exist))
(defgeneric find-module (designator &key if-does-not-exist))
(defgeneric register-module (module))
(defgeneric on-client-connect (module client))

(defun list-modules ()
  (loop for module being the hash-values of *modules*
        collect module))

(defmethod load-module ((designator symbol) &key (if-exists :ignore) (if-does-not-exist :error))
  (let ((module (find-module designator :if-does-not-exist NIL)))
    (when module
      (ecase if-exists
        ((NIL) (return-from load-module NIL))
        (:ignore (return-from load-module module))
        (:error (error 'module-already-exists :designator designator))
        (:reload))))
  (let ((entry-point (find-module-entry-point designator :if-does-not-exist if-does-not-exist)))
    (v:info :forge.module "Loading module for ~a" designator)
    (load-module entry-point :if-exists :reload)))

(defmethod load-module ((designator pathname) &key if-exists if-does-not-exist)
  (declare (ignore if-exists if-does-not-exist))
  (load designator))

(defmethod find-module (designator &key (if-does-not-exist :error))
  (let ((module (gethash (string-downcase designator) *modules*)))
    (or module
        (ecase if-does-not-exist
          ((NIL) NIL)
          (:error (error 'no-such-module :designator designator))
          (:load (load-module designator)
           (find-module designator))))))

(defmethod find-module ((designator module) &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  designator)

(defmethod register-module ((module module))
  (setf (gethash (string-downcase (name module)) *modules*) module))

(defmethod on-client-connect ((module module) (client client)))

(defmethod on-client-connect ((all (eql T)) (client client))
  (dolist (module (list-modules))
    (on-client-connect module client)))

(defmacro define-module (module superclasses slots &rest initargs)
  (let ((instance (gensym "INSTANCE")))
    (remf initargs :class)
    `(progn
       (defclass ,module (,@superclasses module)
         ,slots)
       (let ((,instance (or (find-module ',module :if-does-not-exist NIL)
                            (make-instance ',module :name ',module))))
         (register-module ,instance)
         (reinitialize-instance ,instance ,@initargs)))))

#+asdf
(define-module-entry-point-search-function asdf (designator)
  (asdf:find-system (format NIL "forge-module-~(~a~)" designator) NIL))

#+asdf
(defmethod load-module ((designator asdf:system) &key if-exists if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (unless (find (asdf:component-name designator) (asdf:already-loaded-systems) :test #'string=)
    (ecase if-exists
      ((NIL) (return-from load-module NIL))
      (:error (error 'module-already-exists :designator designator))
      (:reload)))
  (let ((*package* (find-package :cl-user)))
    (asdf:load-system designator :verbose NIL))
  (find-module (asdf:component-name designator)))

