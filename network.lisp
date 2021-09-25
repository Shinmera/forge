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

(defmethod communication:alive-p ((peer peer))
  (communication:alive-p (connection peer)))

(defclass server (peer)
  ((name :initform :server)
   (machine :initform (find-machine :server :if-does-not-exist :create))
   (connection :initform (communication:serve (make-instance 'tcp:host)))

   (machines :initform (make-hash-table :test 'equal) :reader machines)
   (client-info :initform (make-hash-table :test 'equal) :reader client-info)
   (clients :initform (make-hash-table :test 'equal) :reader clients)
   (on-existing-client :initarg :on-existing-client :initform :replace :accessor on-existing-client)))

(defgeneric find-machine (name server &key if-does-not-exist))
(defgeneric (setf find-machine) (machine name server &key if-exists))
(defgeneric delete-machine (name server))
(defgeneric client-info (name server))
(defgeneric (setf client-info) (info name server))
;; This function only works with the server object as we have to use the property
;; of actual real files on the file system to determine changes, which only works
;; on the local instance of the server.
(defgeneric artefact-changed-p (artefact server))

(defmethod find-machine (name (server server) &key (if-does-not-exist :error))
  (or (gethash name (machines server))
      (ecase if-does-not-exist
        ((NIL) NIL)
        (:error (error 'no-such-machine :name name))
        (:create (setf (gethash name (machines server)) (make-instance 'machine :name name))))))

(defmethod (setf find-machine) ((machine machine) name (server server) &key (if-exists :error))
  (when (gethash name *machines*)
    (ecase if-exists
      ((NIL) (return-from (setf find-machine) NIL))
      (:error (error 'machine-already-exists :name name))
      (:replace)))
  (setf (gethash name (machines server)) machine))

(defmethod delete-machine ((machine machine) (server server))
  (delete-machine (name machine) (server server)))

(defmethod delete-machine (name (server server))
  (remhash name *machines*)
  name)

(defmethod client-info (name (server server))
  (or (gethash name (client-info server))
      (error 'no-such-client :name name)))

(defmethod (setf client-info) (info name (server server))
  (setf (gethash name (client-info server))  info))

(defmethod (setf client-info) ((info null) name (server server))
  (remhash name (client-info server)))

(defmethod artefact-changed-p ((artefact artefact) (server server))
  ;; Better would be checking the hash to track changes on sub-second granularity
  ;; or changes that mess with the file timestamp.
  (let ((path (artefact-pathname artefact (machine server))))
    (< (mtime artefact) (file-write-date path))))

(defmethod artefact-changed-p (path (server server))
  (artefact-changed-p (pathname-artefact artefact) server))

(defmethod artefact-changed-p (artefact (server (eql T)))
  (artefact-changed-p artefact *server*))

(defmethod handshake ((server server) (connection connection) (message communication:connect))
  (ecase (communication:version message)
    (0
     (let* ((name (communication:name message))
            (existing (gethash name (clients server)))
            (info (client-info name server)))
       (if existing
           (ecase (on-existing-client server)
             (:replace
              (close existing :abort T)
              (communication:esend connection (make-condition 'client-replaced :name name) message))
             (:error
              (error 'client-already-exists :name name)))
           (communication:reply! connection message 'communication:ok))
       (setf (gethash name (clients server))
             (apply #'make-instance 'client :connection connection :server server :name name info))))))

(defmethod message-loop ((server server))
  (macrolet ((with-message ((message connection) &body body)
               `(let ((,message (communication:receive ,connection :timeout 0.0)))
                  (when ,message
                    ,@body))))
    (let ((connection (connection server))
          (pending ())
          (last-check (get-internal-real-time)))
      (loop ;; Check for incoming connection requests
            (with-message (new-connection connection)
              (push new-connection pending))
            ;; Process pending connections to see if we can upgrade them
            (dolist (connection pending)
              (handler-case
                  (with-message (message connection)
                    (setf pending (remove connection pending))
                    (handler-case
                        (handshake server connection message)
                      (error (e)
                        (communication:esend connection e message)
                        (close connection))))
                (error ()
                  (ignore-errors (close connection))
                  (setf pending (remove connection pending)))))
            ;; Process established client messages
            (loop for client being the hash-values of (clients server)
                  do (handler-case
                         (with-message (message (connection client))
                           (handle message client))
                       (error ()
                         (ignore-errors (close client))))
                     (maintain-connection client))
            ;; Backoff to make sure we don't overheat
            (let ((seconds-passed (/ (- (get-internal-real-time) last-check)
                                     internal-time-units-per-second)))
              (when (< seconds-passed 0.01)
                (sleep (- 0.01 seconds-passed))))
            (setf last-check (get-internal-real-time))))))

(defclass client (peer)
  ((server :initarg :server :initform (support:arg! :server) :reader server)
   (callback-table :initform (make-hash-table :test 'eql) :reader callback-table)
   (last-message :initform (cons (get-universal-time) 0) :accessor last-message)))

(defmethod initialize-instance :after ((client client) &key server)
  (unless (typep (machine client) 'machine)
    (setf (slot-value client 'machine) (find-machine (machine-client) server))))

(defgeneric handle (message client))

(defmethod handle :before ((message communication:message) (client client))
  (setf (last-message client) (cons (get-universal-time) 0)))

(defmethod handle ((message communication:reply) (client client))
  (let ((callback (pophash (communication:id message) (callback-table client))))
    (etypecase callback
      (function (funcall callback message))
      (promise:promise
       (etypecase message
         (communication:error-message
          (promise:fail callback (apply #'make-condition
                                        (communication:condition-type message)
                                        (communication:arguments message))))
         (T
          (promise:succeed callback message)))))))

(defmethod maintain-connection ((client client))
  (let ((timeout (- (get-universal-time) (car (last-message client)))))
    (cond ((< 30 timeout)
           (when (< (cdr (last-message client)) 1)
             (setf (cdr (last-message client)) 1)
             (communication:send! (connection client) 'communication:ping)))
          ((< 75 timeout)
           (when (< (cdr (last-message client)) 2)
             (setf (cdr (last-message client)) 2)
             (communication:send! (connection client) 'communication:ping)))
          ((< 120 timeout)
           (close client)))))

(defmethod close ((client client) &key abort)
  (remhash (name client) (clients (server client)))
  (close (connection client) :abort abort))
