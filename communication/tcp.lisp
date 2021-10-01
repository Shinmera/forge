#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge.communication.tcp
  (:use #:cl)
  (:local-nicknames
   (#:communication #:org.shirakumo.forge.communication)
   (#:socket #:org.shirakumo.forge.support))
  (:export
   #:DEFAULT-PORT
   #:host
   #:address
   #:port
   #:socket
   #:connection
   #:client-connection
   #:server-connection))
(in-package #:org.shirakumo.forge.communication.tcp)

(defconstant DEFAULT-PORT 1984)

(defclass host (communication:host)
  ((address :initarg :address :initform "127.0.0.1" :reader address)
   (port :initarg :port :initform DEFAULT-PORT :reader port)))

(defmethod communication:connect ((host host) machine &key id timeout)
  (let ((socket (socket:open-tcp (address host) (port host) :timeout timeout)))
    (when socket
      (communication:handshake (make-instance 'client-connection :name id :host host :socket socket)
                               machine :id id :timeout timeout))))

(defclass connection (communication:connection)
  ((host :initarg :host :initform (error "HOST required.") :reader communication:host)
   (socket :initarg :socket :initform (error "SOCKET required.") :accessor socket)))

(defmethod communication:alive-p ((connection connection))
  (and (not (null (socket connection)))
       (open-stream-p (socket connection))))

(defmethod close ((connection connection) &key abort)
  (ignore-errors (close (socket connection) :abort abort))
  (setf (socket connection) NIL))

(defclass client-connection (connection communication:client-connection) ())

(defmethod communication:handle :before ((message communication:connection-lost) (connection client-connection))
  (close connection :abort T))

(defmethod communication:receive ((connection client-connection) &key timeout)
  (declare (ignore timeout))
  (communication:decode-message T (socket connection)))

(defmethod communication:send (message (connection client-connection))
  (let ((socket (socket connection)))
    (communication:encode-message message socket)
    (force-output socket)))

(defclass server-connection (connection communication:server-connection)
  ((connections :initform () :accessor connections :reader communication:connections)))
