(defpackage #:org.shirakumo.forge.communication.in-process
  (:use #:cl)
  (:local-nicknames
   (#:communication #:org.shirakumo.forge.communication))
  (:export
   #:host))
(in-package #:org.shirakumo.forge.communication.in-process)

(defclass host (communication:host communication:client-connection communication:server-connection)
  ((state :initform :closed :accessor state)
   (name :initform NIL)
   (queue :reader queue)))

(defmethod initialize-instance :after ((host host) &key)
  (let ((sentinel (cons NIL NIL)))
    (setf (slot-value host 'queue) (cons sentinel sentinel))))

(defmethod communication:connect ((host host) machine &key id timeout)
  (declare (ignore id timeout))
  (ecase (state host)
    (:serving
     (setf (state host) :connected)
     host)
    (:connected
     host)))

(defmethod communication:serve ((host host))
  (ecase (state host)
    (:closed
     (setf (state host) :serving)
     host)
    ((:serving :connected)
     host)))

(defmethod communication:host ((host host))
  host)

(defmethod communication:connections ((host host))
  (list host))

(defmethod communication:alive-p ((host host))
  (not (eql :closed (state host))))

(defmethod communication:handle :before ((message communication:connection-lost) (host host))
  (close host :abort T))

(defmethod close ((host host) &key abort)
  (declare (ignore abort))
  (ecase (state host)
    (:closed)
    (:connected
     (setf (state host) :serving)
     (communication:send (make-instance 'communication:connection-lost :connection host) host))
    (:serving
     (setf (car (queue host)) NIL)
     (setf (cdr (queue host)) NIL)
     (setf (state host) :closed))))

(defmethod communication:send (message (host host))
  (communication:handle message host))

(defmethod communication:receive ((host host) &key timeout)
  (declare (ignore timeout))
  (let ((start (pop (car (queue host)))))
    (unless (car (queue host))
      (setf (cdr (queue host)) NIL))
    start))

(defmethod communication:handle ((command communication:exit) (host host))
  (setf (state host) :closed)
  (when (find-restart 'communication:exit-command-loop)
    (invoke-restart 'communication:exit-command-loop)))
