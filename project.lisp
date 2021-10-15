#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(support:define-condition* no-such-project (error)
  (name version) ("Could not find a project with name~%  ~a~%and matching version~%  ~a" name (to-string (version condition))))

(defvar *projects* (make-hash-table :test 'equal))

(defclass build-effect (effect)
  ())

(defclass project (dependencies-component parent-component)
  ((blueprint :initarg :blueprint :initform *blueprint-truename* :reader blueprint)
   metadata))

(defmethod shared-initialize :after ((project project) slots &key (components NIL components-p))
  (when components-p
    (flet ((find-or-init (type initargs)
             (let ((existing (gethash (getf initargs :name) (children project))))
               (if existing
                   (if (eql type (class-name (class-of existing)))
                       existing
                       (change-class existing type))
                   (apply #'allocate-instance (find-class type) initargs)))))
      ;; First, parse all specs
      (let ((specs (loop for spec in components
                         append (loop for spec in (normalize-component-spec project spec)
                                      collect (parse-component project spec))))
            (old-table (children project))
            (children (make-hash-table :test 'equal)))
        ;; Next update/allocate the entire table, this should avoid triggering
        ;; shared-initialize on any passed initargs.
        (loop for (type . initargs) in specs
              for component = (find-or-init type initargs)
              do (setf (gethash (name component) children) component))
        (setf (children project) children)
        ;; Finally now that all component objects are known, reinitialize to
        ;; trigger shared-initialize methods on initargs that might need to resolve.
        ;; FIXME: How do we also undo effects created in the db?
        ;;        Need some kinda transactioning...
        ;;        Generally also need to clean up leftover effects when a project is
        ;;        redefined though, wonder how to do that nicely.
        (with-cleanup-on-unwind (setf (children project) old-table)
          (loop for spec in specs
                for component = (gethash (getf (rest spec) :name) children)
                do (apply #'reinitialize-instance component (rest spec))))))))

(defmethod make-step ((operation operation) (project project) (effect build-effect))
  (make-instance 'compound-step
                 :operation operation
                 :component project
                 :effect effect
                 :inner-effect (in-order-to effect project)))

(defgeneric ensure-version (version-ish))
(defgeneric parse-project (module project-definition))
(defgeneric find-project (name &key version if-does-not-exist))
(defgeneric register-project (project &optional source-path))
(defgeneric delete-project (project))
(defgeneric in-order-to (operation project))
(defgeneric build (project &key policy executor))
(defgeneric normalize-component-spec (project spec))
(defgeneric parse-component (project spec))
(defgeneric default-component-type (project))
(defgeneric default-project-type (module))

(defmethod normalize-component-spec ((project project) spec)
  (enlist spec))

(defmethod parse-component ((project project) spec)
  (destructuring-bind (name . args) spec
    (let ((type (getf args :type (default-component-type project))))
      (remf args :type)
      (list* type :name name args))))

(defun list-projects ()
  (let ((projects ()))
    (loop for versions being the hash-values of *projects*
          when (typep versions 'list)
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

(defmethod find-project ((name string) &key (version (parse-constraint T)) (if-does-not-exist :error))
  (let* ((name (string-downcase name))
         (versions (gethash name *projects*)))
    (or (loop for project in versions
              when (version-match-p (version project) version)
              return project)
        (ecase if-does-not-exist
          ((NIL) (return-from find-project NIL))
          (:error (error 'no-such-project :name name :version version))))))

(defmethod find-project ((name symbol) &rest args)
  (apply #'find-project (string name) args))

(defmethod register-project ((project project) &optional source-path)
  (let* ((name (string-downcase (name project)))
         (versions (gethash name *projects*))
         (existing-path (gethash project *projects*)))
    (with-simple-restart (abort "Don't register the new project.")
      (when source-path
        (when (and existing-path (not (equal existing-path source-path)))
          (warn 'project-source-path-changed :new source-path :old existing-path))
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
    (destructuring-bind (type name version initargs) (parse-project module args)
      `(let* ((*blueprint-truename* ,*blueprint-truename*)
              (,versiong (ensure-version ,version))
              (,instance (or (find-project ',name :version ,versiong :if-does-not-exist NIL)
                             (make-instance ',type :name ,name :version ,versiong))))
         (reinitialize-instance ,instance ,@initargs)
         (register-project ,instance ,(or *blueprint-truename* *compile-file-truename* *load-truename*))))))

(defmethod build (project &rest args)
  (apply #'build (find-project project :if-does-not-exist :error) args))

(defmethod build ((project project) &key (policy 'basic-policy) (executor 'linear-executor) (effect-type 'build-effect))
  (let* ((effect (find-effect *database* effect-type (name project) (version project) T))
         (plan (compute-plan effect (ensure-instance policy 'policy)))
         (executor (ensure-instance executor 'executor)))
    (execute plan executor)))

(defmethod parse-project ((module module) project-definition)
  (let ((type (getf project-definition :type (default-project-type module)))
        (name (getf project-definition :name)))
    (check-type type (and symbol (not null)))
    (check-type name string)
    (list type
          name
          (or (getf project-definition :version) 0)
          (loop for (key val) on (removef project-definition :type :name :version) by #'cddr
                do (check-type key symbol)
                collect key collect `',val))))

(define-module forge ()
  ())

(defmethod default-project-type ((module forge))
  'project)


(defclass artefact-project (project)
  ((registry :accessor registry)))

(defmethod shared-initialize ((project artefact-project) slots &key)
  (call-next-method)
  (unless (slot-boundp project 'registry)
    (setf (registry project) (find-registry (name project) *server* :if-does-not-exist (blueprint project)))))

(defmethod default-component-type ((project artefact-project))
  'artefact-component)

(defmethod normalize-component-spec ((project artefact-project) component)
  (let* ((registry (registry project))
         (root (path registry)))
    (destructuring-bind (file . args) (enlist component)
      (if (wild-pathname-p file)
          (loop for file in (directory (merge-pathnames file root))
                collect (list* (enough-namestring file root) args))
          (list (list* file args))))))

(defmethod parse-component ((project artefact-project) spec)
  (destructuring-bind (path . args) spec
    (let ((type (getf args :type (default-component-type project)))
          (name (getf args :name path))
          (version (getf args :version (version project)))
          (artefact (find-artefact path (registry project) :if-does-not-exist :create)))
      (list* type
             :name name
             :artefact artefact
             :version version
             :parent project
             (removef args :type :name :artefact :version)))))
