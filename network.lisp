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

(defmethod find-registry (name (peer peer) &rest args)
  (apply #'find-registry name (machine peer) args))

(defmethod (setf find-registry) (registry name (peer peer) &rest args)
  (apply #'(setf find-registry) registry name (machine peer) args))

(defmethod artefact-pathname (artefact (peer peer))
  (artefact-pathname artefact (machine peer)))

(defmethod pathname-artefact (artefact (peer peer) &rest args)
  (apply #'pathname-artefact artefact (machine peer) args))

(defmethod find-artefact (designator (peer peer) &rest args &key &allow-other-keys)
  (apply #'find-artefact designator (machine peer) args))

(defmethod delete-artefact (designator (peer peer))
  (delete-artefact designator (machine peer)))

(defclass server (peer)
  ((name :initform :server)
   (machine :initform NIL)
   (connection :initform NIL)

   (machines :initform (make-hash-table :test 'equal) :reader machines)
   (clients :initform (make-hash-table :test 'equal) :reader clients)
   (on-existing-client :initarg :on-existing-client :initform :replace :accessor on-existing-client)
   (message-thread :initform NIL :accessor message-thread)
   (promise-thread :initform NIL :accessor promise-thread)
   (running-p :initform T :accessor running-p)))

(defmethod initialize-instance :after ((server server) &key)
  (unless (machine server)
    (setf (slot-value server 'machine) (find-machine (machine-instance) server :if-does-not-exist :create)))
  (setf *server* server))

(defgeneric start (server &key))
(defgeneric stop (server))
(defgeneric list-clients (server))
(defgeneric find-machine (name server &key if-does-not-exist))
(defgeneric (setf find-machine) (machine name server &key if-exists))
(defgeneric delete-machine (name server))
(defgeneric artefact-changed-p (artefact server))
(defgeneric message-loop (server))
(defgeneric promise-loop (server))

(defmethod start ((default (eql T)) &rest initargs &key &allow-other-keys)
  (let ((start-args ()) (init-args ()))
    (loop for (key value) on initargs by #'cddr
          do (cond ((find key '(:listen :address :port :if-exists))
                    (push value start-args)
                    (push key start-args))
                   (T
                    (push value init-args)
                    (push key init-args))))
    (unless *server*
      (apply #'make-instance 'server init-args))
    (apply #'start *server* start-args)))

(defmethod start ((server server) &key (listen :tcp) (address "127.0.0.1") (port tcp:DEFAULT-PORT) (if-exists :error))
  (case (communication:alive-p server)
    ((T)
     (ecase if-exists
       (:error (error "server is already running."))
       ((NIL) (return-from start NIL))
       (:supersede (stop server))))
    ((NIL) (v:info :forge.network "Starting ~a..." server)))
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
  (setf (running-p server) T)
  (unless (and (message-thread server) (bt:thread-alive-p (message-thread server)))
    (setf (message-thread server)
          (bt:make-thread (lambda () (message-loop server))
                          :name (format NIL "forge-~(~a~)-message-thread" (name server)))))
  (unless (and (promise-thread server) (bt:thread-alive-p (promise-thread server)))
    (setf (promise-thread server)
          (bt:make-thread (lambda () (promise-loop server))
                          :name (format NIL "forge-~(~a~)-promise-thread" (name server)))))
  server)

(defmethod stop ((default (eql T)))
  (stop *server*))

(defmethod stop ((server server))
  (when (communication:alive-p server)
    (v:info :forge.network "Stopping ~a..." server)
    (setf (running-p server) NIL)
    (unwind-protect
         (progn
           (when (message-thread server) (wait-for-thread-exit (message-thread server)))
           (when (promise-thread server) (wait-for-thread-exit (promise-thread server))))
      (close (connection server))
      (setf (slot-value server 'connection) NIL)))
  server)

(defmethod list-clients ((server server))
  (alexandria:hash-table-values (clients server)))

(defmethod communication:alive-p ((server server))
  (let ((message (and (message-thread server) (bt:thread-alive-p (message-thread server))))
        (promise (and (promise-thread server) (bt:thread-alive-p (promise-thread server)))))
    (cond ((and message promise) T)
          ((or message promise) :weird)
          (T NIL))))

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

(defmethod artefact-changed-p ((artefact artefact) (server server))
  (let ((path (artefact-pathname artefact (machine server))))
    (or (< (mtime artefact) (file-write-date path))
        (with-open-file (stream path :element-type '(unsigned-byte 8))
          (or (/= (size artefact) (file-length path))
              (not (equal (hash artefact) (hash-file stream))))))))

(defmethod artefact-changed-p (path (server server))
  (artefact-changed-p (pathname-artefact path server) server))

(defmethod artefact-changed-p (artefact (server (eql T)))
  (artefact-changed-p artefact *server*))

(defmethod handshake ((server server) (connection communication:connection) (message communication:connect))
  (case (communication:version message)
    (0
     (let* ((name (list (communication:machine message) (or (communication:client-id message) (random #xFFFF))))
            (machine (find-machine (communication:machine message) server))
            (existing (gethash name (clients server))))
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
       (let ((client (make-instance 'client :connection connection :server server :name name :machine machine)))
         (v:info :forge.network "Established new client connection ~a" client)
         (setf (gethash name (clients server)) client)
         (on-client-connect T client)
         client)))
    (T
     (error 'unsupported-protocol-version :version (communication:version message)))))

(defmethod message-loop ((server server))
  (v:debug :forge.network "Entering message loop for ~a" server)
  (macrolet ((with-message ((message connection) &body body)
               `(let ((,message (communication:receive ,connection :timeout 0.0)))
                  (when ,message
                    ,@body))))
    (unwind-protect
         (with-event-loop ((connection (connection server))
                           (pending ()))
           (unless (running-p server)
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
                 do (support:handler-case*
                        (with-message (message client)
                          (handle message client))
                      (error (e)
                        (v:debug :forge.network "Encountered error handling message: ~a" e)
                        (v:trace :forge.network e)
                        (ignore-errors (close client))))
                    (maintain-connection client)))
      (v:debug :forge.network "Leaving message loop for ~a" server))))

(defmethod promise-loop ((server server))
  (v:debug :forge.network "Entering promise loop for ~a" server)
  (unwind-protect
       (with-event-loop ()
         (unless (running-p server)
           (return))
         (with-simple-restart (abort "Abort ticking")
           (support:handler-case*
               (promise:tick-all (get-universal-time))
             (error (e)
                    (v:debug :forge.network "Encountered error ticking promises: ~a" e)
                    (v:trace :forge.network e)))))
    (v:debug :forge.network "Leaving promise loop for ~a" server)))

(defmacro with-promise ((server) &body body)
  `(promise:then (promise:pend :success T)
                 (lambda (_)
                   (declare (ignore _))
                   ,@body)))

(defclass client (peer)
  ((server :initarg :server :initform (support:arg! :server) :reader server)
   (callback-table :initform (make-hash-table :test 'eql) :reader callback-table)
   (last-message :initform (cons (get-universal-time) 0) :accessor last-message)))

(defmethod initialize-instance :after ((client client) &key server)
  (unless (typep (machine client) 'machine)
    (setf (slot-value client 'machine) (find-machine (machine-client) server))))

(defgeneric handle (message client))

(defmethod communication:send (message (client client))
  (v:trace :forge.network "~a <-- ~a" (name client) message)
  (communication:send message (connection client)))

(defmethod communication:receive ((client client) &key timeout)
  (let ((message (communication:receive (connection client) :timeout timeout)))
    (when message
      (v:trace :forge.network "~a --> ~a" (name client) message))
    message))

(defmethod handle :before ((message communication:message) (client client))
  (setf (last-message client) (cons (get-universal-time) 0)))

(defmethod handle ((message communication:reply) (client client))
  (let ((callback (pophash (communication:id message) (callback-table client))))
    (etypecase callback
      (null)
      (function (funcall callback message))
      (promise:promise
       (etypecase message
         (communication:error-message
          (promise:fail callback (apply #'make-condition
                                        (communication:condition-type message)
                                        (communication:arguments message))))
         (T
          (promise:succeed callback message)))))))

(defmethod handle ((message communication:effect-request) (client client))
  (promise:-> (with-promise ((server client))
                (handler-bind ((error #'invoke-debugger))
                  (let* ((effect (find-effect *database*
                                              (communication:effect-type message)
                                              (communication:parameters message)
                                              (parse-constraint (communication:version message))))
                         (plan (compute-plan effect (make-instance 'basic-policy)))
                         (executor (ecase (communication:execute-on message)
                                     (:self (make-instance 'linear-executor :client client))
                                     (:any (make-instance 'linear-executor :client (alexandria:random-elt (clients (server client)))))
                                     (:all (make-instance 'parallel-executor)))))
                    (execute plan executor))))
    (:then () (communication:reply! client message 'communication:ok))
    (:handle (e) (communication:esend client e message))))

(defmethod handle ((message communication:exit) (client client))
  (close client))

(defun promise-reply (message client &key (lifetime 120) send values-list)
  (let ((promise (promise:pend :lifetime lifetime)))
    (setf (gethash (communication:id message) (callback-table client)) promise)
    (when send (communication:send message client))
    (promise:then promise (lambda (v) (promise:pend :success (if values-list
                                                                 (communication:value v)
                                                                 (first (communication:value v))))))))

(defmacro with-client-eval ((client &key (lifetime 120) values-list) &body body)
  `(promise-reply (make-instance 'communication:eval-request :form (progn ,@body))
                  ,client :lifetime ,lifetime :values-list ,values-list :send T))

(defmethod maintain-connection ((client client))
  (let ((timeout (- (get-universal-time) (car (last-message client)))))
    (cond ((< 30 timeout)
           (when (< (cdr (last-message client)) 1)
             (setf (cdr (last-message client)) 1)
             (communication:send! client 'communication:ping)))
          ((< 75 timeout)
           (v:debug :forge.network "Connection unstable: no reply in 75 seconds for ~a..."  client)
           (when (< (cdr (last-message client)) 2)
             (setf (cdr (last-message client)) 2)
             (communication:send! client 'communication:ping)))
          ((< 120 timeout)
           (v:warn :forge.network "Dropping client ~a as we have not received a message in 2 minutes." client)
           (close client)))))

(defmethod close ((client client) &key abort)
  (v:info :forge.network "Closing connection to ~a" client)
  (remhash (name client) (clients (server client)))
  (close (connection client) :abort abort))

;; KLUDGE: patch decoding of artefacts here to ensure we get actual artefact instances instead of
;;         just references to them through the communications protocol.
(defmethod communication:decode-message ((id (eql (communication:encoding-type-id 'communication:artefact))) (stream stream))
  (let ((registry (communication:decode-message T stream))
        (path (communication:decode-message T stream))
        (machine (communication:decode-message T stream)))
    (find-artefact path (find-machine machine *server*) :registry registry)))
