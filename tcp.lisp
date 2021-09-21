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

(defmethod communication:receive ((server server-connection) &key timeout)
  (when (or (null timeout) (usocket:wait-for-input (socket server) :timeout timeout :ready-only T))
    (let ((socket (usocket:socket-accept (socket server) :element-type '(unsigned-byte 8))))
      (when socket
        (let ((client (make-instance 'client-connection :host (communication:host server) :socket socket)))
          (push client (connections server))
          (make-instance 'communication:connection-established :connection client))))))
