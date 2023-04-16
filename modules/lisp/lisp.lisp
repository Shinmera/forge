#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.modules.lisp)

(stealth-mixin:define-stealth-mixin client () forge:client
  ((load-tracking :initform (make-hash-table :test 'equal) :accessor load-tracking)))

(forge:define-module lisp ()
  ())

(defmethod forge:default-project-type ((module lisp))
  'project)

(defmethod forge:on-client-connect ((module lisp) (client forge:client))
  (promise:then
   (forge:with-client-eval (client)
     `(let ((intern (intern "ORG.SHIRAKUMO.FORGE.MODULES.LISP.LOAD-TRACKING" "CL-USER")))
        (if (boundp intern)
            (symbol-value intern)
            (set intern (make-hash-table :test 'equal)))))
   (lambda (cached)
     (clrhash (load-tracking client))
     (loop for k being the hash-keys of cached
           do (setf (gethash k (load-tracking client)) T)))))

(defun implementation-version-string ()
  (load-time-value
   (format NIL "~a-~a-~a-~a"
           (software-type)
           (machine-type)
           (lisp-implementation-type)
           (lisp-implementation-version))))

(defclass compile-effect (forge:effect) ())
(defclass load-effect (forge:effect) ())

(defclass file (forge:child-component forge:dependencies-component forge:file-component)
  ())

(defmethod forge:supported-operations append ((file file))
  '(load-operation compile-file-operation load-fasl-operation))

(defmethod forge:normalize-dependency-spec ((file file) dep)
  (let ((component (gethash dep (forge:children (forge:parent file)))))
    (unless component
      (error "Fuck"))
    (forge:normalize-dependency-spec file component)))

(defmethod forge:normalize-dependency-spec ((file file) (dependency file))
  (forge: dependency))

(defclass lisp-source-operation (forge:operation)
  ((verbose :initarg :verbose :initform NIL :accessor verbose)))

(defmethod forge:dependencies append ((op lisp-source-operation) (component file))
  (loop for dependency in (forge:depends-on component)
        collect (forge:depend 'load-effect dependency :version (forge:version component))))

(defclass load-operation (lisp-source-operation)
  ())

(defmethod forge:make-effect ((op load-operation) (component file))
  (forge:ensure-effect op component 'load-effect (forge:artefact component)))

(defmethod forge:perform ((op load-operation) (component file) client)
  (let ((path (forge:artefact-pathname component client))
        (time (forge:mtime (forge:artefact component))))
    (promise:then (forge:with-client-eval (client)
                    `(progn (load ,path
                                  :verbose ,(verbose op)
                                  :print ,(verbose op))
                            (setf (gethash ,path 'cl-user::org.shirakumo.forge.modules.lisp.load-tracking) ,time)))
                  (lambda (_)
                    (setf (gethash path (load-tracking client)) time)))))

(defclass compile-file-operation (lisp-source-operation)
  ())

(defmethod forge:make-effect ((op compile-file-operation) (component file))
  (call-next-method)
  (forge:ensure-effect op component 'compile-effect (forge:artefact component)))

(defmethod forge:output-file-type ((op compile-file-operation) (component file))
  "fasl")

(defmethod forge:perform ((op compile-file-operation) (component file) client)
  (let ((output (forge:artefact-pathname (forge:realize-artefact (forge:output-artefact op component) op) client)))
    (forge:with-client-eval (client)
      `(compile-file ,(forge:artefact-pathname component client)
                     :output-file (ensure-directories-exist ,output)
                     :verbose ,(verbose op)
                     :print ,(verbose op)))))

(defmethod forge:perform :before ((step forge:step) (op compile-file-operation) client)
  (setf (forge:forced-p step) T))

(defclass load-fasl-operation (forge:compiler-input-operation lisp-compiler-operation)
  ())

(defmethod forge:make-effect ((op load-fasl-operation) (component file))
  (forge:ensure-effect op component 'load-effect (forge:artefact component)))

(defmethod forge:input-file-type ((op load-fasl-operation) (component file))
  "fasl")

(defmethod forge:perform ((op load-fasl-operation) (component file) client)
  (let ((path (forge:artefact-pathname component client))
        (time (forge:mtime (forge:artefact component))))
    (promise:then (forge:with-client-eval (client)
                    `(progn (load ,(forge:artefact-pathname (forge:realize-artefact (forge:input-artefact op component) op) client))
                            (setf (gethash ,path 'cl-user::org.shirakumo.forge.modules.lisp.load-tracking) ,time)))
                  (lambda (_)
                    (setf (gethash path (load-tracking client)) time)))))

(defclass load-into-image-operation (forge:operation)
  ())

(defclass project (forge:artefact-project)
  ())

(defmethod forge:supported-operations append ((project project))
  '(load-operation load-into-image-operation))

(defmethod forge:make-effect ((op load-into-image-operation) (project project))
  (forge:ensure-effect op project 'forge::build-effect (forge:name project)))

(defmethod forge:normalize-dependency-spec ((project project) spec)
  (let ((spec (forge::enlist spec)))
    (flet ((parse-project-spec (name &key (effect 'load-effect) (version T) weak)
             (forge:depend effect
                           (string-downcase name)
                           :version (forge:parse-constraint version)
                           :hard (not weak)))
           (parse-effect-spec (type parameters &key (version T) (hard T))
             (forge:depend type
                           parameters
                           :version (forge:parse-constraint version)
                           :hard hard)))
      (case (first spec)
        (:effect
         (apply #'parse-effect-spec (rest spec)))
        (:project
         (apply #'parse-project-spec (rest spec)))
        (T
         (apply #'parse-project-spec spec))))))

(defmethod forge:dependencies append ((op load-into-image-operation) (project project))
  (forge:depends-on project))

(defmethod forge:make-effect ((op load-operation) (project project))
  (forge:ensure-effect op project 'load-effect (forge:name project)))

(defmethod forge:in-order-to ((effect forge::build-effect) (project project))
  (forge:find-effect T 'load-effect (forge:name project) (forge:version project)))

(defmethod forge:dependencies append ((op load-operation) (project project))
  (loop for component being the hash-values of (forge:children project)
        collect (forge:depend 'load-effect (forge:artefact component)
                              :version (forge:version component))))

(defmethod forge:default-component-type ((project project))
  'file)

(defmethod forge:perform ((op load-operation) (project project) client)
  (promise:pend))
