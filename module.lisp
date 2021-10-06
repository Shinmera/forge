#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

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

(defvar *modules* (make-hash-table :test 'eql))

(defclass module ()
  ((name :initarg :name :initform (support:arg! :name) :reader name)))

(defgeneric load-module (designator &key if-exists if-does-not-exist))
(defgeneric find-module (designator &key if-does-not-exist))
(defgeneric register-module (module))

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
    (load-module entry-point :if-exist :reload)))

(defmethod load-module ((designator pathname) &key if-exists if-does-not-exist)
  (declare (ignore if-exists if-does-not-exist))
  (load designator))

(defmethod find-module ((designator symbol) &key (if-does-not-exist :error))
  (let ((module (gethash designator *modules*)))
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
  (setf (gethash (name module) *modules*) module))

(defmacro define-module (module superclasses slots &rest initargs)
  (let ((instance (gensym "INSTANCE")))
    (remf initargs :class)
    `(progn
       (defclass ,module (,@superclasses module)
         ,slots)
       (let ((,instance (or (find-module ',module :if-does-not-exist NIL)
                            (make-instance ',class :name ',module))))
         (register-module ,instance)
         (reinitialize-instance ,instance ,@initargs)))))

#+asdf
(define-module-entry-point-search-function asdf (designator)
  (asdf:find-system (format NIL "forge-module-~a" designator) NIL))

#+asdf
(defmethod load-module ((designator asdf:system) &key if-exists if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (unless (asdf:needed-in-image-p 'asdf:load-op designator)
    (ecase if-exists
      ((NIL) (return-from load-module NIL))
      (:error (error 'module-already-exists :designator designator))
      (:reload)))
  (asdf:load-system designator :force T :verbose NIL))

