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

(defclass file (forge:file-component)
  ())

(defmethod forge:supported-operations append ((file file))
  '(load-operation compile-file-operation load-fasl-operation))

(defclass lisp-source-operation (forge:operation)
  ((verbose :initarg :verbose :initform NIL :accessor verbose)))

(defclass load-operation (lisp-source-operation)
  ())

(defmethod forge:make-effect ((op load-operation) (c file))
  (forge:ensure-effect op c 'load-effect (forge:file c)))

(defmethod forge:perform ((op load-operation) (c file) client)
  (forge:eval-on client `(load ,(forge:local-file (forge:file c) client)
                               :verbose ,(verbose op)
                               :print ,(verbose op))))

(defclass compile-file-operation (lisp-source-operation)
  ())

(defmethod forge:make-effect ((op compile-file-operation) (c file))
  (forge:ensure-effect op c 'compile-effect (forge:file c)))

(defmethod forge:output-file ((op compile-file-operation) (c file))
  (let* ((file (cdr (forge:file c)))
         (dir (list* :relative
                     (implementation-version-string)
                     (id (forge:project c))
                     (forge:to-string (forge:version c))
                     (rest (pathname-directory file)))))
    (cons :cache (make-pathname :name (pathname-name file) :type "fasl" :directory dir))))

(defmethod forge:perform ((op compile-file-operation) (c file) client)
  (forge:with-eval (client `(compile-file ,(forge:local-file (forge:file c) client)
                                          :output-file ,(forge:local-file (forge:output-file op c) client)
                                          :verbose ,(verbose op)
                                          :print ,(verbose op)))
    (file)
    (forge:notice-artefact file client)))

(defclass load-fasl-operation (forge:operation)
  ())

(defmethod forge:make-effect ((op load-fasl-operation) (c file))
  (forge:ensure-effect op c 'load-effect (forge:file c)))

(defmethod forge:dependencies append ((op load-fasl-operation) (c lisp-file))
  (list (forge:depend 'compile-effect (forge:file c))))

(defmethod forge:perform ((op load-fasl-operation) (c file) client)
  (forge:eval-on client `(load ,(forge:local-file (forge:output-file 'compile-file-operation c) client))))

(defclass compile-effect (forge:effect) ())
(defclass load-effect (forge:effect) ())
