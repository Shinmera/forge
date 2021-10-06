#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defvar *projects* (make-hash-table :test 'equal))

(defclass parent-component (component)
  ((children :initform (make-hash-table :test 'equal) :reader children)))

(defclass project (parent-component)
  ((name :initarg :name :initform (support:arg! :name) :reader name)
   (blueprint :initarg :blueprint :initform *blueprint-truename* :reader blueprint)
   metadata))

(defmethod make-step ((operation operation) (project project) (effect effect))
  (make-instance 'compound-step
                 :operation operation
                 :component component
                 :effect effect
                 :inner-effect (in-order-to operation project)))

(defgeneric in-order-to (operation project))
(defgeneric ensure-version (version-ish))

(defgeneric parse-project (module project-definition))
(defgeneric find-project (name &key version if-does-not-exist))
(defgeneric register-project (project &optional source-path))
(defgeneric delete-project (project))

(defun list-projects ()
  (let ((projects ()))
    (loop for versions being the hash-values of *projects*
          do (loop for project in versions
                   do (push project projects)))
    projects))

(defmethod ensure-version ((version version))
  version)

(defmethod ensure-version ((version string))
  (version-from-string version))

(defmethod ensure-version (version-ish)
  (parse-version version-ish))

(defmethod ensure-version ((file pathname))
  (version-from-string (alexandria:read-file-into-string file)))

(defmethod find-project ((name string) &key (version (constraint T)) (if-does-not-exist :error))
  (let* ((name (string-downcase name))
         (versions (gethash name *projects*)))
    (or (loop for project in versions
              when (version-match-p (version project) constraint)
              return project)
        (ecase if-does-not-exist
          ((NIL) (return-from find-project NIL))
          (:error (error 'no-such-project :name name))))))

(defmethod find-project ((name symbol) &rest args)
  (apply #'find-project (string name) args))

(defmethod register-project ((project project) &optional source-path)
  (let* ((name (string-downcase name))
         (versions (gethash name *projects*))
         (existing-path (gethash project *projects*)))
    (with-simple-restart (abort "Don't register the new project.")
      (when source-path
        (when (and existing-path (not (equal existing-path source-path)))
          (warn 'project-source-path-changed :new source-path :old (second entry)))
        (setf (gethash project *projects*) source-path))
      (pushnew project versions)
      (setf (gethash name *projects*) versions))
    project))

(defmethod delete-project (name)
  (let ((versions (gethash name *projects*)))
    (remhash (string-downcase name) *projects*)
    (dolist (project versions)
      (remhash project *projects*))
    name))

(defmethod delete-project ((project project))
  (let* ((name (string-downcase (name project)))
         (versions (remove project (gethash name *projects*))))
    (remhash project *projects*)
    (if versions
        (setf (gethash name *projects*) versions)
        (remhash name *projects*))
    name))

(defmacro define-project (modules &body args)
  (let ((module (or (first (mapcar #'load-module modules)) (find-module 'forge)))
        (versiong (gensym "VERSION"))
        (instance (gensym "INSTANCE")))
    (destructuring-bind (type name version . initargs) (parse-project module args)
      `(let* ((*blueprint-truename* ,*blueprint-truename*)
              (,versiong (ensure-version ,version))
              (,instance (or (find-project ',name :version ,versiong :if-does-not-exist NIL)
                             (make-instance ',type :name ,name :version ,versiong))))
         (reinitialize-instance ,instance ,@initargs)
         (register-project ,instance ,(or *blueprint-truename* *compile-file-truename* *load-truename*))))))

(define-module forge ()
  ())

(defmethod parse-project ((forge forge) project-definition)
  (list (or (getf project-definition :type) 'project)
        (or (getf project-definition :name) (support:arg! :name))
        (or (getf project-definition :version) 0)
        (removef project-definition :type :name :version)))
