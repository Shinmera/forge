#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.communication)

(defvar *version* ())
(defvar *id-counter* 0)

;; Init to something hopefully unique on this machine
(defun init-id-counter (&optional (machine-id (sxhash (machine-instance))))
  (let ((*random-state* (make-random-state T)))
    (setf *id-counter* (+ (ash (ldb (byte 32 0) machine-id) 32)
                          (ash (ldb (byte 16 0) (get-universal-time)) 16)
                          (ash (ldb (byte 16 0) (random #xFFFF)) 0)))))

(defun next-id ()
  (incf *id-counter*))

(defclass host () ())
(defgeneric connect (host &key timeout)) ; => CONNECTION
(defgeneric serve (host)) ; => CONNECTION
(defgeneric connections (host)) ; => (CONNECTION)

(defclass connection () ())
(defgeneric host (connection)) ; => HOST
(defgeneric alive-p (connection)) ; => BOOLEAN
(defgeneric send (message connection))
(defgeneric receive (connection &key timeout)) ; => MESSAGE | NIL
(defgeneric handle (message connection))

(defun send! (connection type &rest args)
  (send (apply #'make-instance type args) connection))

(define-compiler-macro send! (connection type &rest args)
  `(send (make-instance ,type ,@args) ,connection))

(defun reply! (connection message type &rest args)
  (send (apply #'make-instance type :id (id message) args) connection))

(define-compiler-macro reply! (connection message type &rest args)
  `(send (make-instance ,type :id (id ,message) ,@args) ,connection))

(defclass client-connection (connection) ())
(defclass server-connection (connection) ())

(defclass message ()
  ((id :initarg :id :initform (next-id) :reader id)))

(defclass connection-established (message) ())

(defmethod handle ((message connection-established) (connection connection)))

(defclass connection-lost (message) ())

(defmethod handle ((message connection-lost) (connection connection)))

(defclass command (message) ())
(defclass ok (message) ())

(defclass exit (command) ())

(defmethod handle ((request exit) (connection connection))
  (invoke-restart 'exit-command-loop))

(defclass ping (message)
  ((clock :initform (get-universal-time) :reader clock)))

(defclass pong (message)
  ((clock :initform (get-universal-time) :reader clock)))

(defmethod handle ((request ping) (connection connection))
  (reply! connection request 'pong))

(defclass error-message (message)
  ((condition-type :initarg :condition-type :initform (support:arg! :condition-type) :reader condition-type)
   (arguments :initarg :arguments :initform () :reader arguments)
   (report :initarg :report :initform NIL :reader report)))

(defun esend (connection error &optional message)
  (send (make-instance 'error-message :condition-type (type-of error)
                                      :arguments (support:arguments error)
                                      :report (princ-to-string error)
                                      :id (if message (id message) (next-id)))
        connection))

(defclass eval-request (command)
  ((form :initarg :form :initform (support:arg! :form) :reader form)))

(defclass return-message (message)
  ((value :initarg :value :initform (support:arg! :value) :reader value)))

(defmethod handle ((request eval-request) (connection connection))
  (let ((values (multiple-value-list (eval (form request)))))
    (reply! connection request 'return-message :value values)))

;; Class for a client to request a plan and execution.
(defclass effect-request (command)
  ((effect-type :initarg :effect-type :initform (support:arg! :effect-type) :reader effect-type)
   (parameters :initarg :parameters :initform (support:arg! :parameters) :reader parameters)
   (version :initarg :version :initform (support:arg! :version) :reader version)
   (execute-on :initarg :execute-on :initform :self :reader execute-on)))

(defstruct (artefact
            (:constructor make-artefact (source target))
            (:copier NIL)
            (:predicate NIL))
  (source NIL :type pathname :read-only T)
  (target NIL :type pathname :read-only T))

(defstruct (dummy-symbol
            (:constructor make-dummy-symbol (package name))
            (:copier NIL)
            (:predicate NIL))
  (package NIL :type string :read-only T)
  (name NIL :type string :read-only T))

(defgeneric encode-message (message stream))
(defgeneric decode-message (type stream))

(defun handle1 (connection &key timeout)
  (handler-case
      (let ((message (receive connection :timeout timeout)))
        (when message
          (handler-case (handle message connection)
            (error (e)
              (esend connection e message)))))
    (error (e)
      (esend connection e))))
