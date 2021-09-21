#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.communication)

(defvar *timeout* NIL)
(defvar *version* ())
(defvar *id-counter* 0)

(defun next-id ()
  (incf *id-counter*))

(defmacro with-timeout (timeout &body body)
  `(let ((*timeout* ,timeout))
     ,@body))

(defclass host () ())
(defgeneric connect (host &key timeout)) ; => CONNECTION
(defgeneric serve (host &key timeout)) ; => CONNECTION
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
(defclass connection-lost (message) ())
(defclass command (message) ())
(defclass exit (command) ())
(defclass ok (message) ())

(defclass ping (message)
  ((clock :initform (get-universal-time) :reader clock)))

(defclass pong (message)
  ((clock :initform (get-universal-time) :reader clock)))

(defclass error-message (message)
  ((condition-type :initarg :condition-type :initform (support:arg! :condition-type) :reader condition-type)
   (arguments :initarg :arguments :initform () :reader arguments)
   (report :initarg :report :initform NIL :reader report)))

(defclass eval-request (command)
  ((form :initarg :form :initform (support:arg! :form) :reader form)))

(defclass return-message (message)
  ((value :initarg :value :initform (support:arg! :value) :reader value)))

(defmethod arguments (error)
  ())

(defmethod handle ((request eval-request) (connection connection))
  (let ((values (multiple-value-list (eval (form request)))))
    (reply! connection request 'return-message :value values)))

(defstruct (artefact
            (:constructor make-artefact (source target))
            (:copier NIL)
            (:predicate NIL))
  (source NIL :type pathname :read-only T)
  (target NIL :type pathname :read-only T))

(defgeneric encode-message (message stream))
(defgeneric decode-message (type stream))

(defun command-loop (connection)
  (with-simple-restart (exit-command-loop "Exit processing commands")
    (flet ((receive ()
             (handler-case (receive connection)
               (error ()
                 (handle (make-instance 'connection-lost) connection)
                 (invoke-restart 'exit-command-loop)))))
      (loop (restart-case
                (let ((message (receive)))
                  (handler-case (handle message connection)
                    (error (e)
                      (reply! connection message 'error-message
                              :condition-type (type-of e)
                              :condition-arguments (arguments e)
                              :report (princ-to-string e)))))
              (reconnect ()
                :report (lambda (s) "Reconnect to ~a" (host connection))
                ;; FIXME: What to do if reconnection fails?
                (connect (host connection)))
              (continue ()
                :report "Ignore the message and continue processing."
                NIL))))))
