#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge.protocol.tcp
  (:use #:cl)
  (:local-nicknames
   (#:protocol #:org.shirakumo.forge.protocol))
  (:export
   #:host
   #:connection
   #:client-connection
   #:server-connection))

(in-package #:org.shirakumo.forge.protocol.tcp)

(defun open-tcp (host port &key (element-type '(unsigned-byte 8)))
  #+allegro
  (excl:make-socket :remote-host host :remote-port port)
  #+abcl
  (let ((socket (system:make-socket host port)))
    (system:get-socket-stream socket :element-type element-type))
  #+ccl
  (ccl:make-socket :remote-host host :remote-port port)
  #+(or clasp ecl sbcl mkcl)
  (let* ((endpoint (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host)))
         (socket (make-instance 'sb-bsd-sockets:inet-socket :protocol :tcp :type :stream)))
    (sb-bsd-sockets:socket-connect socket endpoint port)
    (sb-bsd-sockets:socket-make-stream socket
                                       :element-type element-type
                                       :input T :output T
                                       :buffering :full))
  #+clisp
  (socket:socket-connect port host :element-type element-type)
  #+(or cmucl scl)
  (let ((fd (extensions:connect-to-inet-socket host port)))
    (extensions:make-fd-stream fd :element-type element-type
                                  :input T :output T))
  #+lispworks
  (comm:open-tcp-stream host port :element-type element-type
                                  :direction :io
                                  :errorp T
                                  :read-timeout NIL
                                  :timeout 5)
  #-(or allegro abcl ccl clasp ecl sbcl mkcl clisp cmucl scl lispworks)
  (error "Your implementation is not supported."))

(defun listen-tcp (host port)
  #+(or clasp ecl sbcl mkcl)
  (let* ((endpoint (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host)))
         (socket (make-instance 'sb-bsd-sockets:inet-socket :protocol :tcp :type :stream)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) T)
    (sb-bsd-sockets:socket-bind server endpoint port)
    (sb-bsd-sockets:socket-listen server 16)
    socket)
  #-(or clasp ecl sbcl mkcl)
  (error "Your implementation is not supported."))

(defun accept-tcp (socket &key timeout (element-type '(unsigned-byte 8)))
  ;; FIXME: Handle timeout
  #+(or clasp ecl sbcl mkcl)
  (sb-bsd-sockets:socket-make-stream (sb-bsd-sockets:socket-accept socket)
                                     :element-type element-type
                                     :input T :output T
                                     :buffering :full))

(defclass host (protocol:host)
  ((address :initarg :address :initform "0.0.0.0" :reader address)
   (port :initarg :port :initform 5001 :reader port)))

(defmethod protocol:open ((host host))
  (let ((socket (open-tcp (address host) (port host))))
    (make-instance 'client-connection :host host :socket socket)))

(defmethod protocol:serve ((host host))
  (let ((socket (listen-tcp (address host) (port host))))
    (make-instance 'server-connection :host host :socket socket)))

(defclass connection (protocol:connection)
  ((host :initarg :host :initform (error "HOST required.") :reader protocol:host)
   (socket :initarg :socket :initform (error "SOCKET required.") :accessor socket)))

(defmethod protocol:alive-p ((connection connection))
  (not (null (socket connection))))

(defmethod protocol:close ((connection connection))
  (ignore-errors (cl:close (socket connection)))
  (setf (socket connection) NIL))

(defclass client-connection (connection protocol:client-connection) ())

(defmethod read ((connection client-connection) &key timeout)
  ;; FIXME: handle timeout
  (protocol:decode-message (socket connection)))

(defmethod write (message (connection client-connection))
  (protocol:encode-message (socket connection)))

(defclass server-connection (connection protocol:server-connection)
  ((connections :initform () :accessor connections)))

(defmethod read ((server server-connection) &key timeout)
  (let ((socket (accept-tcp (socket server) :timeout timeout)))
    (when socket
      (let ((client (make-instance 'client-connection :host (protocol:host connection) :socket socket)))
        (push client (protocol:connections server))
        (make-instance 'protocol:connection-established :connection client)))))
