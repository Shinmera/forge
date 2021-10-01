#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.modules.lisp)

(defun implementation-version-string ()
  (load-time-value
   (format NIL "~a-~a-~a-~a"
           (software-type)
           (machine-type)
           (lisp-implementation-type)
           (lisp-implementation-version))))

(defclass artefact-effect (forge:effect) ())
(defclass compile-effect (forge:effect) ())
(defclass load-effect (forge:effect) ())

(defclass file (forge:component)
  ((file :initarg :file :reader file)
   (project :initarg :project :reader project)
   (forge:version :initform (load-time-value (make-instance 'forge:integer-version)))))

(defmethod artefact ((file file) client)
  (forge:find-artefact (file file) client :registry (project file)))

(defmethod forge:supported-operations append ((file file))
  '(ensure-artefact-operation load-operation compile-file-operation load-fasl-operation))

(defclass ensure-artefact-operation (forge:operation)
  ())

(defmethod forge:make-effect ((op ensure-artefact-operation) (c file))
  (forge:ensure-effect op c 'artefact-effect c))

(defmethod forge:perform ((op ensure-artefact-operation) (c file) client)
  (let ((artefact (artefact c forge:*server*)))
    (when (forge:artefact-changed-p artefact client)
      (promise:-> (forge:with-client-eval (client)
                    (communication::make-artefact (forge:artefact-pathname artefact forge:*server*)
                                                  (forge:artefact-pathname artefact client)))
        (:then (file) (forge:notice-file file client))))))

(defclass lisp-source-operation (forge:operation)
  ((verbose :initarg :verbose :initform NIL :accessor verbose)))

(defmethod forge:dependencies append ((op lisp-source-operation) (c file))
  (list (forge:depend 'artefact-effect c)))

(defclass load-operation (lisp-source-operation)
  ())

(defmethod forge:make-effect ((op load-operation) (c file))
  (forge:ensure-effect op c 'load-effect (file c)))

(defmethod forge:perform ((op load-operation) (c file) client)
  (forge:with-client-eval (client)
    `(load ,(forge:artefact-pathname (artefact c client) client)
           :verbose ,(verbose op)
           :print ,(verbose op))))

(defclass compile-file-operation (lisp-source-operation)
  ())

(defmethod forge:make-effect ((op compile-file-operation) (c file))
  (forge:ensure-effect op c 'compile-effect (file c)))

(defmethod output-artefact ((op compile-file-operation) (c file))
  (let ((dir (list* :relative
                    (implementation-version-string)
                    (princ-to-string (project c))
                    (forge:to-string (forge:version c))
                    (rest (pathname-directory (file c))))))
    (forge:find-artefact (make-pathname :name (pathname-name (file c)) :type "fasl" :directory dir)
                         forge:*server* :if-does-not-exist :create)))

(defmethod forge:perform ((op compile-file-operation) (c file) client)
  (promise:-> (forge:with-client-eval (client)
                `(compile-file ,(forge:artefact-pathname (artefact c client) client)
                               :output-file (ensure-directories-exist ,(forge:artefact-pathname (output-artefact op c) client))
                               :verbose ,(verbose op)
                               :print ,(verbose op)))
    (:then (file) (forge:notice-file file client))))

(defclass load-fasl-operation (forge:operation)
  ())

(defmethod forge:make-effect ((op load-fasl-operation) (c file))
  (forge:ensure-effect op c 'load-effect (file c)))

(defmethod forge:dependencies append ((op load-fasl-operation) (c file))
  (list (forge:depend 'compile-effect (file c))))

(defmethod forge:perform ((op load-fasl-operation) (c file) client)
  (forge:with-client-eval (client)
    `(load ,(forge:artefact-pathname (output-artefact 'compile-file-operation c) client))))
