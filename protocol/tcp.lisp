#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge.protocol.tcp
  (:use #:cl)
  (:local-nicknames
   (#:protocol #:org.shirakumo.forge.protocol)
   (#:socket #:org.shirakumo.forge.support))
  (:export
   #:DEFAULT-PORT
   #:host
   #:connection
   #:client-connection
   #:server-connection))
(in-package #:org.shirakumo.forge.protocol.tcp)

(defconstant DEFAULT-PORT 1984)

(defclass host (protocol:host)
  ((address :initarg :address :initform "0.0.0.0" :reader address)
   (port :initarg :port :initform DEFAULT-PORT :reader port)))

(defmethod protocol:connect ((host host) &key timeout)
  (let ((socket (socket:open-tcp (address host) (port host) :timeout timeout)))
    (when socket
      (make-instance 'client-connection :host host :socket socket))))

(defmethod protocol:serve ((host host) &key timeout)
  (let ((socket (socket:listen-tcp (address host) (port host) :timeout timeout)))
    (when socket
      (make-instance 'server-connection :host host :socket socket))))

(defclass connection (protocol:connection)
  ((host :initarg :host :initform (error "HOST required.") :reader protocol:host)
   (socket :initarg :socket :initform (error "SOCKET required.") :accessor socket)))

(defmethod protocol:alive-p ((connection connection))
  (not (null (socket connection))))

(defmethod close ((connection connection) &key abort)
  (ignore-errors (close (socket connection) :abort abort))
  (setf (socket connection) NIL))

(defclass client-connection (connection protocol:client-connection) ())

(defmethod protocol:receive ((connection client-connection) &key timeout)
  (protocol:with-timeout timeout
    (protocol:decode-message T (socket connection))))

(defmethod protocol:send (message (connection client-connection))
  (protocol:encode-message message (socket connection)))

(defclass server-connection (connection protocol:server-connection)
  ((connections :initform () :accessor connections)))

(defmethod protocol:receive ((server server-connection) &key timeout)
  (let ((socket (socket:accept-tcp (socket server) :timeout timeout)))
    (when socket
      (let ((client (make-instance 'client-connection :host (protocol:host connection) :socket socket)))
        (push client (protocol:connections server))
        (make-instance 'protocol:connection-established :connection client)))))
