#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge.protocol.in-process
  (:use #:cl)
  (:local-nicknames
   (#:protocol #:org.shirakumo.forge.protocol))
  (:export
   #:host))
(in-package #:org.shirakumo.forge.protocol.in-process)

(defclass host (protocol:host protocol:client-connection protocol:server-connection)
  ((state :initform :closed :accessor state)
   (queue :reader queue)))

(defmethod initialize-instance :after ((host host) &key)
  (let ((sentinel (cons NIL NIL))))
  (setf (slot-value host 'queue) (cons sentinel sentinel)))

(defmethod protocol:connect ((host host))
  (ecase (state host)
    (:serving
     (setf (state host) :connected)
     host)
    (:connected
     host)))

(defmethod protocol:serve ((host host))
  (ecase (state host)
    (:closed
     (setf (state host) :serving)
     host)
    ((:serving :connected)
     host)))

(defmethod protocol:host ((host host))
  host)

(defmethod protocol:connections ((host host))
  (list host))

(defmethod protocol:alive-p ((host host))
  (not (eql :closed (state host))))

(defmethod close ((host host))
  (ecase (state host)
    (:connected
     (write (make-instance 'protocol:connection-lost :connection host) host)
     (setf (state host) :serving))
    (:serving
     (setf (car (queue host)) NIL)
     (setf (cdr (queue host)) NIL)
     (setf (state host) :closed))))

(defmethod protocol:write (message (host host))
  (let ((cons (cons message NIL)))
    (if (cdr (queue host))
        (setf (cddr (queue host)) cons)
        (setf (car (queue host)) (cdr (queue host)) cons))))

(defmethod protocol:read ((host host) &key timeout)
  (declare (ignore timeout))
  (let ((start (pop (car (queue host)))))
    (unless (car (queue host))
      (setf (cdr (queue host)) NIL))
    start))

(defmethod protocol:send ((command protocol:command) (host host))
  (protocol:execute command))
