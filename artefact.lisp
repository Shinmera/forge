#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(support:define-condition* no-such-registry (error)
  (name machine) ("A registry named~%  ~s~%could not be found on the machine~%  ~s" name (machine condition)))

(support:define-condition* registry-already-exists (error)
  (name machine) ("A registry named~%  ~s~%already exists on the machine~%  ~s" name (machine condition)))

(support:define-condition* no-such-artefact (error)
  (path registry) ("An artefact at path~%  ~a~%could not be found in the registry~%  ~s" path (registry condition)))

;; Early-def
(defclass registry () ())
(defclass artefact () ())

(defclass machine ()
  ((name :initarg :name :initform (support:arg! :name) :reader name)
   (registries :initform (make-hash-table :test 'equal) :reader registries)))

(define-print-object-method* machine "~s" name)

(defgeneric find-registry (name machine &key if-does-not-exist))
(defgeneric (setf find-registry) (registry name machine &key if-exists))
(defgeneric artefact-pathname (artefact machine))
(defgeneric pathname-artefact (pathname machine &key if-does-not-exist))

(defmethod find-registry (name (machine machine) &key (if-does-not-exist :error))
  (or (gethash name (registries machine))
      (etypecase if-does-not-exist
        (null NIL)
        ((eql :error)
         (error 'no-such-registry :name name :machine machine))
        ((or string pathname)
         (setf (find-registry name machine) if-does-not-exist)))))

(defmethod (setf find-registry) ((registry registry) name (machine machine) &key (if-exists :error))
  (when (gethash name (registries machine))
    (ecase if-exists
      ((NIL) (return-from find-registry NIL))
      (:error (error 'registry-already-exists :name name :machine machine))
      (:replace)))
  (setf (gethash name (registries machine)) registry))

(defmethod (setf find-registry) ((path string) name (machine machine) &key (if-exists :error))
  (setf (find-registry name machine :if-exists if-exists) (make-instance 'registry :path path :name name)))

(defmethod (setf find-registry) ((path pathname) name (machine machine) &key (if-exists :error))
  (let ((path (directory-namestring (truename (ensure-directories-exist path)))))
    (setf (find-registry name machine :if-exists if-exists) path)))

(defmethod artefact-pathname ((artefact artefact) (machine machine))
  (let ((registry (find-registry (registry artefact) machine :if-does-not-exist :error)))
    (merge-pathnames (path artefact) (path registry))))

(defmethod pathname-artefact ((path string) (machine machine) &key (if-does-not-exist :error))
  (let ((longest "") (winner NIL))
    (loop for registry being the hash-values of (registries machine)
          for base = (path registry)
          do (when (and (< (length longest) (length base))
                        (< (length base) (length path))
                        (string= base path :end2 (length base)))
               (setf longest base)
               (setf winner registry)))
    (unless winner
      (ecase if-does-not-exist
        ((NIL) (return-from pathname-artefact NIL))
        (:error (error 'no-such-registry :name path :machine machine))))
    (let ((sub (subseq path (length longest))))
      (find-artefact sub winner :if-does-not-exist if-does-not-exist))))

(defmethod pathname-artefact ((path pathname) (machine machine) &key (if-does-not-exist :error))
  (pathname-artefact (namestring path) machine :if-does-not-exist if-does-not-exist))

(defclass registry ()
  ((name :initarg :name :initform (support:arg! :name) :reader name)
   (artefacts :initform (make-hash-table :test 'equal) :reader artefacts)
   (path :initarg :path :initform (support:arg! :path) :reader path)))

(define-print-object-method* registry "~s" name)

(defgeneric find-artefact (path registry &key if-does-not-exist))
(defgeneric delete-artefact (designator registry))
(defgeneric register-artefact (artefact registry))

(defmethod find-artefact ((artefact artefact) (registry registry) &rest args)
  (apply #'find-artefact (path artefact) registry args))

(defmethod find-artefact ((path string) (registry registry) &key (if-does-not-exist :error))
  (or (gethash path (artefacts registry))
      (ecase if-does-not-exist
        ((NIL) NIL)
        (:error (error 'no-such-artefact :path path :registry registry))
        (:create (setf (gethash path (artefacts registry)) (make-instance 'artefact :registry (name registry) :path path))))))

(defmethod find-artefact ((path pathname) (registry registry) &rest args)
  (apply #'find-artefact (namestring path) registry args))

(defmethod find-artefact (path (machine machine) &key (registry :cache) (if-does-not-exist :error))
  (find-artefact path (find-registry registry machine) :if-does-not-exist if-does-not-exist))

(defmethod find-artefact ((artefact artefact) (machine machine) &rest args)
  (apply #'find-artefact artefact (find-registry (registry artefact) machine :if-does-not-exist :error) args))

(defmethod delete-artefact ((path string) (registry registry))
  (remhash path (artefacts registry))
  path)

(defmethod delete-artefact ((artefact artefact) (registry registry))
  (remhash (path artefact) (artefacts registry))
  artefact)

(defmethod register-artefact ((artefact artefact) (registry registry))
  (unless (equal (name registry) (registry artefact))
    (error "Cannot register artefact on registry it does not belong to!"))
  (let ((existing (gethash (path artefact) (artefacts registry))))
    (when (and existing (not (eq artefact existing)))
      (cerror "Replace the artefact." "Cannot put~%  ~a~%into~%  ~a~%as it already has~%  ~a"
              artefact registry existing))
    (setf (gethash (path artefact) (artefacts registry)) artefact)))

(defclass artefact ()
  ((path :initarg :path :initform (support:arg! :path) :reader path)
   (registry :initarg :registry :initform (support:arg! :registry) :reader registry)
   (size :initarg :size :initform NIL :accessor size)
   (hash :initarg :hash :initform NIL :accessor hash)
   (mtime :initarg :mtime :initform NIL :accessor mtime)))

(define-print-object-method* artefact "~s ~s" (registry artefact) path)

(defmethod probe ((artefact artefact))
  (with-open-file (stream (artefact-pathname artefact *server*) :element-type '(unsigned-byte 8))
    (setf (size artefact) (file-length stream))
    (setf (mtime artefact) (file-write-date stream))
    (setf (hash artefact) (hash-file stream)))
  artefact)

(defmethod artefact-supersedes-p ((newer? artefact) (older? artefact))
  (or (artefact-changed-p newer? *server*)
      (< (mtime newer?) (mtime older?))))
