#|
This file is a part of forge
(c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

;; Patchup for server side with usocket.

(in-package #:org.shirakumo.forge.communication.tcp)

(defmethod communication:serve ((host host))
  (let ((socket (usocket:socket-listen (address host) (port host)
                                       :reuse-address T
                                       :element-type '(unsigned-byte 8))))
    (when socket
      (make-instance 'server-connection :host host :socket socket))))

(defmethod communication:connect ((host host) machine &key id timeout)
  (let ((socket (usocket:socket-connect (address host) (port host)
                                        :timeout timeout
                                        :element-type '(unsigned-byte 8))))
    (communication:handshake (make-instance 'client-connection :name id :host host :socket socket)
                             machine :id id :timeout timeout)))

(defmethod communication:receive ((server server-connection) &key timeout)
  (when (or (null timeout) (usocket:wait-for-input (socket server) :timeout timeout :ready-only T))
    (let* ((socket (usocket:socket-accept (socket server) :element-type '(unsigned-byte 8)))
           (client (make-instance 'client-connection :name NIL :host (communication:host server) :socket socket)))
      (push client (connections server))
      client)))

(defmethod communication:receive ((connection client-connection) &key timeout)
  (when (or (null timeout) (usocket:wait-for-input (socket connection) :timeout timeout :ready-only T))
    (communication:decode-message T (usocket:socket-stream (socket connection)))))

(defmethod communication:send (message (connection client-connection))
  (let ((stream (usocket:socket-stream (socket connection))))
    (communication:encode-message message stream)
    (force-output stream)))

(defmethod close ((connection server-connection) &key abort)
  (declare (ignore abort))
  (usocket:socket-close (socket connection)))

(defmethod close ((connection client-connection) &key abort)
  (declare (ignore abort))
  (usocket:socket-close (socket connection)))
