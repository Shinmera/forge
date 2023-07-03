;;;; Description:
;;; This file defines the blueprint mechanism, which is the way by which
;;; users define projects and their components for Forge. Blueprints by
;;; themselves are merely files with the fixed name "blueprint", which
;;; contain a number of project definition expressions.

(in-package #:org.shirakumo.forge)

(defvar *blueprints* (make-hash-table :test 'equal))
(defvar *blueprint-search-paths* ())
(defvar *blueprint* NIL)

(support:define-condition* no-such-blueprint (error)
  (path) ("Could not find a registered blueprint with path~%  ~a" path))

(defclass blueprint ()
  ((path :initform NIL :accessor path)
   (hash :initarg :hash :initform NIL :accessor hash)
   (projects :initform (make-hash-table :test 'equalp) :accessor projects)))

(defmethod shared-initialize :after ((blueprint blueprint) slots &key path hash)
  (when path
    (setf (path blueprint) path))
  (when (and path (null hash))
    (setf (hash blueprint) (hash-file path))))

(defmethod print-object ((blueprint blueprint) stream)
  (print-unreadable-object (blueprint stream :type T)
    (format stream "~a" (path blueprint))))

(defmethod make-load-form ((blueprint blueprint) &optional env)
  (declare (ignore env))
  `(blueprint ,(path blueprint)))

(defmethod (setf path) ((path pathname) (blueprint blueprint))
  (let ((old (path blueprint)))
    (setf (slot-value blueprint 'path) (truename path))
    (when old (setf (blueprint old) NIL))
    (setf (blueprint path) blueprint)))

(defmethod blueprint ((path pathname))
  (or (gethash (truename path) *blueprints*)
      (restart-case (error 'no-such-blueprint :path path)
        (load-blueprint ()
          :report "Try to load the file as a blueprint"
          (load-blueprint path)))))

(defmethod (setf blueprint) ((blueprint blueprint) (path pathname))
  (setf (gethash (truename path) *blueprints*) blueprint))

(defmethod (setf blueprint) ((null null) (path pathname))
  (remhash path *blueprints*))

(defun list-blueprints ()
  (alexandria:hash-table-values *blueprints*))

(defmethod load-blueprint-file ((blueprint blueprint) path)
  ;; FIXME: use Eclector to catch attempts at introducing symbols into external packages
  (with-standard-io-syntax
    (let* ((hash (hash-file path))
           (blueprint-package (make-blueprint-package))
           (*package* blueprint-package)
           (*read-eval* NIL))
      (unwind-protect
           (with-open-file (stream path :direction :input :external-format :utf-8)
             (loop with *blueprint* = blueprint
                   for form = (read stream NIL '#1=#:eof)
                   until (eq form '#1#)
                   do (etypecase form
                        (cons
                         (handle-blueprint-form (car form) (cdr form)))))
             (setf (hash blueprint) hash)
             blueprint)
        (ignore-errors (delete-package blueprint-package))))))

(defmethod load-blueprint ((blueprint blueprint) &key force)
  (when (or force (string/= (hash blueprint) (hash-file (path blueprint))))
    (let ((temp (tempfile :type "lisp")))
      ;; We copy the file out to ensure that changes to the original while loading
      ;; don't impact the load and are properly detected as new changes when
      ;; attempting to load the file again.
      (uiop:copy-file (path blueprint) temp)
      (restart-case (load-blueprint-file blueprint temp)
        (abort ()
          :report "Ignore the failed load"
          NIL)
        (remove ()
          :report "Remove the blueprint"
          (setf (blueprint (path blueprint)) NIL)
          NIL)))))

(defmethod load-blueprint ((path pathname) &key force)
  (let ((blueprint (gethash path *blueprints*)))
    (unless blueprint
      (setf blueprint (make-instance 'blueprint :path path :hash "")))
    (load-blueprint blueprint :force force)))

(defun add-blueprint-search-path (path &key discover if-exists)
  (loop for existing in *blueprint-search-paths*
        do (when (pathname-utils:subpath-p path existing)
             (ecase if-exists
               ((NIL) (return-from add-blueprint-search-path NIL))
               (:error (error 'blueprint-search-path-exists :path path :existing existing)))))
  (push path *blueprint-search-paths*)
  (when discover
    (load-blueprints (discover-blueprints (list path))))
  path)

(defun discover-blueprint-files (&optional (paths *blueprint-search-paths*))
  (let ((paths ()))
    (dolist (path paths paths)
      (scan-directory (truename path) "blueprint" (lambda (path) (push path paths))))))

(defun load-blueprints (&key (paths (discover-blueprint-files)) force)
  (loop for path in paths
        for value = (load-blueprint path :force force)
        when value collect value))

(defun reload-blueprints (&key force)
  (loop for blueprint being the hash-values of *blueprints*
        do (load-blueprint blueprint :force force)))

(defun make-blueprint-package ()
  (let ((package (make-package (format NIL "ORG.SHIRAKUMO.FORGE.BLUEPRINT.~a" (random-id)) :use ())))
    (sb-ext:add-package-local-nickname "FORGE" #.*package* package)
    package))

(defgeneric handle-blueprint-form (operator args))
