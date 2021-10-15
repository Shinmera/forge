#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.modules.lisp)

(forge:define-module lisp ()
  ())

(defmethod forge:default-project-type ((module lisp))
  'project)

(defun implementation-version-string ()
  (load-time-value
   (format NIL "~a-~a-~a-~a"
           (software-type)
           (machine-type)
           (lisp-implementation-type)
           (lisp-implementation-version))))

(defclass compile-effect (forge:effect) ())
(defclass load-effect (forge:effect) ())

(defclass file (forge:child-component forge:dependencies-component forge:artefact-component)
  ((forge:version :initform (load-time-value (make-instance 'forge:integer-version)))))

(defmethod forge:supported-operations append ((file file))
  '(load-operation compile-file-operation load-fasl-operation))

(defmethod forge:normalize-dependency-spec ((file file) dep)
  (let ((component (gethash dep (forge:children (forge:parent file)))))
    (unless component
      (error "Fuck"))
    (forge:artefact component)))

(defclass lisp-compiler-operation (forge:compiler-operation)
  ())

(defmethod forge:select-compiler ((op lisp-compiler-operation) (policy forge:basic-policy))
  ;; FIXME: need more info on the client's available compiler here...
  (make-instance 'forge:compiler :name (lisp-implementation-type)
                                 :version (forge:version-from-string (lisp-implementation-version))))

(defclass lisp-source-operation (lisp-compiler-operation)
  ((verbose :initarg :verbose :initform NIL :accessor verbose)))

(defmethod forge:dependencies append ((op lisp-source-operation) (component file))
  (loop for dependency in (forge:depends-on component)
        collect (forge:depend 'load-effect dependency :version (forge:version component))))

(defclass load-operation (lisp-source-operation)
  ())

(defmethod forge:make-effect ((op load-operation) (component file))
  (forge:ensure-effect op component 'load-effect (forge:artefact component)))

(defmethod forge:perform ((op load-operation) (component file) client)
  (forge:with-client-eval (client)
    `(load ,(forge:artefact-pathname component client)
           :verbose ,(verbose op)
           :print ,(verbose op))))

(defclass compile-file-operation (forge:compiler-output-operation lisp-source-operation)
  ())

(defmethod forge:make-effect ((op compile-file-operation) (component file))
  (call-next-method)
  (forge:ensure-effect op component 'compile-effect (forge:artefact component)))

(defmethod forge:output-file-type ((op compile-file-operation) (component file))
  "fasl")

(defmethod forge:perform ((op compile-file-operation) (component file) client)
  (forge:with-client-eval (client)
    `(compile-file ,(forge:artefact-pathname component client)
                   :output-file (ensure-directories-exist ,(forge:artefact-pathname (forge:output-artefact op component) client))
                   :verbose ,(verbose op)
                   :print ,(verbose op))))

(defclass load-fasl-operation (forge:compiler-input-operation lisp-compiler-operation)
  ())

(defmethod forge:make-effect ((op load-fasl-operation) (component file))
  (forge:ensure-effect op component 'load-effect (forge:artefact component)))

(defmethod forge:input-file-type ((op load-fasl-operation) (component file))
  "fasl")

(defmethod forge:perform ((op load-fasl-operation) (component file) client)
  (forge:with-client-eval (client)
    `(load ,(forge:artefact-pathname (forge:realize-artefact (forge:input-artefact op component) op) client))))

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
