#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defvar *server* NIL)

(support:define-condition* no-such-client (error)
  (name server) ("No client with the name~%  ~s~%is registered on~%  ~a" name server))

(support:define-condition* no-such-machine (error)
  (name server) ("No machine with the name~%  ~s~%is registered on~%  ~a" name server))

(defclass peer ()
  ((name :initarg :name :initform (support:arg! :name) :reader name)
   (machine :initarg :machine :initform (support:arg! :machine) :reader machine)
   (connection :initarg :connection :initform (support:arg! :connection) :reader connection)))

(defmethod print-object ((peer peer) stream)
  (print-unreadable-object (peer stream :type T)
    (format stream "~s ~:[DEAD~;ALIVE~]" (name peer) (communication:alive-p peer))))

(defmethod communication:alive-p ((peer peer))
  (communication:alive-p (connection peer)))

(defclass server (peer)
  ((name :initform :server)
   (machine :initform NIL)
   (connection :initform NIL)

   (machines :initform (make-hash-table :test 'equal) :reader machines)
   (client-info-table :initform (make-hash-table :test 'equal) :reader client-info-table)
   (clients :initform (make-hash-table :test 'equal) :reader clients)
   (on-existing-client :initarg :on-existing-client :initform :replace :accessor on-existing-client)
   (message-thread :initform NIL :accessor message-thread)
   (running-p :initform T :accessor running-p)))

(defmethod initialize-instance :after ((server server) &key)
  (unless (machine server)
    (setf (slot-value server 'machine) (find-machine :server server :if-does-not-exist :create)))
  (setf *server* server))

(defmethod start ((server server) &key (listen :tcp) (address "127.0.0.1") (port tcp:DEFAULT-PORT))
  (when (communication:alive-p server)
    (error "server is already running."))
  (unless (and (connection server) (communication:alive-p (connection server)))
    (setf (slot-value server 'connection)
          (communication:serve
           (ecase listen
             (:tcp
              (v:info :forge.network "Listening on ~a:~a" address port)
              (make-instance 'tcp:host :address address :port port))
             (:in-process
              (v:info :forge.network "Starting in-process communication")
              (make-instance 'in-process:host))))))
  (v:info :forge.network "Starting ~a..." server)
  (setf (running-p server) T)
  (setf (message-thread server)
        (bt:make-thread (lambda () (message-loop server))
                        :name (format NIL "forge-~(~a~)-message-thread" (name server)))))

(defmethod stop ((server server))
  (when (communication:alive-p server)
    (let ((thread (message-thread server)))
      (v:info :forge.network "Stopping ~a..." server)
      (setf (running-p server) NIL)
      (loop for i from 0
            do (unless (bt:thread-alive-p thread)
                 (return))
               (when (<= 10 i)
                 (restart-case (error "Message thread is not shutting down!")
                   (interrupt (&optional (function #'break))
                     :report "Try to interrupt the thread."
                     (bt:interrupt-thread thread function))
                   (abort ()
                     :report "Kill and forget the thread."
                     (bt:destroy-thread thread)
                     (setf (message-thread server) NIL))
                   (continue ()
                     :report "Continue waiting.")))
               (sleep 0.1)))
    (close (connection server))))

(defmethod communication:alive-p ((server server))
  (and (message-thread server)
       (bt:thread-alive-p (message-thread server))))

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
        (:error (error 'no-such-machine :server server :name name))
        (:create (setf (gethash name (machines server)) (make-instance 'machine :name name))))))

(defmethod (setf find-machine) ((machine machine) name (server server) &key (if-exists :error))
  (when (gethash name *machines*)
    (ecase if-exists
      ((NIL) (return-from find-machine NIL))
      (:error (error 'machine-already-exists :name name))
      (:replace)))
  (setf (gethash name (machines server)) machine))

(defmethod delete-machine ((machine machine) (server server))
  (delete-machine (name machine) (server server)))

(defmethod delete-machine (name (server server))
  (remhash name *machines*)
  name)

(defmethod client-info (name (server server))
  (or (gethash name (client-info-table server))
      (error 'no-such-client :server server :name name)))

(defmethod (setf client-info) (info name (server server))
  (setf (gethash name (client-info-table server))  info))

(defmethod (setf client-info) ((info null) name (server server))
  (remhash name (client-info-table server)))

(defmethod artefact-changed-p ((artefact artefact) (server server))
  ;; Better would be checking the hash to track changes on sub-second granularity
  ;; or changes that mess with the file timestamp.
  (let ((path (artefact-pathname artefact (machine server))))
    (< (mtime artefact) (file-write-date path))))

(defmethod artefact-changed-p (path (server server))
  (artefact-changed-p (pathname-artefact artefact) server))

(defmethod artefact-changed-p (artefact (server (eql T)))
  (artefact-changed-p artefact *server*))

(defmethod handshake ((server server) (connection communication:connection) (message communication:connect))
  (case (communication:version message)
    (0
     (let* ((name (communication:name message))
            (existing (gethash name (clients server)))
            (info (client-info name server)))
       (v:debug :forge.network "Attempted connection establishment for ~a" name)
       (setf (slot-value connection 'communication:name) name)
       (if existing
           (ecase (on-existing-client server)
             (:replace
              (v:debug :forge.network "Replacing connection of duplicate client ~a" name)
              (close existing :abort T)
              (communication:esend connection (make-condition 'client-replaced :name name) message))
             (:error
              (error 'client-already-exists :name name)))
           (communication:reply! connection message 'communication:ok))
       (let ((client (apply #'make-instance 'client :connection connection :server server :name name info)))
         (v:info :forge.network "Established new client connection ~a" client)
         (setf (gethash name (clients server)) client))))
    (T
     (error 'unsupported-protocol-version :version (communication:version message)))))

(defmethod message-loop ((server server))
  (v:debug :forge.network "Entering message loop for ~a" server)
  (macrolet ((with-message ((message connection) &body body)
               `(let ((,message (communication:receive ,connection :timeout 0.0)))
                  (when ,message
                    ,@body))))
    (unwind-protect
         (let ((connection (connection server))
               (pending ())
               (last-check (get-internal-real-time)))
           (loop (unless (running-p server)
                   (return))
                 ;; Check for incoming connection requests
                 (with-message (new-connection connection)
                   (v:debug :forge.network "New incoming connection at ~a" connection)
                   (push (list new-connection (get-universal-time)) pending))
                 ;; Process pending connections to see if we can upgrade them
                 (loop for (connection start-time) in pending
                       do (support:handler-case*
                              (with-message (message connection)
                                (setf pending (remove connection pending :key #'first))
                                (handler-case
                                    (handshake server connection message)
                                  (error (e)
                                    (v:debug :forge.network "Encountered error during handshake: ~a" e)
                                    (v:trace :forge.network e)
                                    (communication:esend connection e message)
                                    (close connection))))
                            (error (e)
                                   (v:debug :forge.network "Encountered weird message during connection establishment.")
                                   (v:trace :forge.network e)
                                   (ignore-errors (close connection))
                                   (setf pending (remove connection pending))))
                          ;; Drop connections that are just dos-ing us.
                          (when (< 30 (- (get-universal-time) start-time))
                            (v:debug :forge.network "Dropping connection ~a: handshake timeout" connection)
                            (close connection)
                            (setf pending (remove connection pending :key #'first))))
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
                 (setf last-check (get-internal-real-time))))
      (v:debug :forge.network "Leaving message loop for ~a" server))))

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
           (v:debug :forge.network "Connection unstable: no reply in 75 seconds for ~a..."  client)
           (when (< (cdr (last-message client)) 2)
             (setf (cdr (last-message client)) 2)
             (communication:send! (connection client) 'communication:ping)))
          ((< 120 timeout)
           (v:warn :forge.network "Dropping client ~a as we have not received a message in 2 minutes." client)
           (close client)))))

(defmethod close ((client client) &key abort)
  (v:info :forge "Closing connection to ~a" client)
  (remhash (name client) (clients (server client)))
  (close (connection client) :abort abort))
