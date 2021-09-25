#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defvar *server* NIL)

(defclass peer ()
  ((name :initarg :name :initform (support:arg! :name) :reader name)
   (machine :initarg :machine :initform (support:arg! :machine) :reader machine)
   (connection :initarg :connection :initform (support:arg! :connection) :reader connection)))

(defclass server (peer)
  ((name :initform :server)
   (machine :initform (find-machine :server :if-does-not-exist :create))
   (clients :initform (make-hash-table :test 'equal) :accessor clients)
   (connection :initform (communication:serve (make-instance 'tcp:host)))))

;; This function only works with the server object as we have to use the property
;; of actual real files on the file system to determine changes, which only works
;; on the local instance of the server.
(defgeneric artefact-changed-p (artefact server))

(defmethod artefact-changed-p ((artefact artefact) (server server))
  ;; Better would be checking the hash to track changes on sub-second granularity
  ;; or changes that mess with the file timestamp.
  (let ((path (artefact-pathname artefact (machine server))))
    (< (mtime artefact) (file-write-date path))))

(defmethod artefact-changed-p (path (server server))
  (artefact-changed-p (pathname-artefact artefact) server))

(defmethod artefact-changed-p (artefact (server (eql T)))
  (artefact-changed-p artefact *server*))

(defclass client (peer)
  ())
