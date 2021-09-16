#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.protocol)

(defvar *timeout* NIL)
(defvar *version* ())

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

(defclass client-connection (connection) ())
(defclass server-connection (connection) ())

(defclass message () ())
(defclass connection-established (message) ())
(defclass connection-lost (message) ())
(defclass command (message) ())
(defclass exit (command) ())

(defclass eval-request (command)
  ((id :initarg :id :initform 0 :reader id)
   (form :initarg :form :initform (error "FORM required") :reader form)))

(defclass return-message (message)
  ((id :initarg :id :initform (error "ID required") :reader id)
   (value :initarg :value :initform (error "VALUE required") :reader value)))

(defclass error-message (message)
  ((id :initarg :id :initform (error "ID required") :reader id)
   (condition-type :initarg :condition-type :initform (error "CONDITION-TYPE") :reader condition-type)
   (arguments :initarg :arguments :initform () :reader arguments)
   (report :initarg :report :initform NIL :reader report)))

(defmethod arguments (error)
  ())

(defmethod handle ((request eval-request) (connection connection))
  (handler-case
      (let ((values (multiple-value-list (eval (form request)))))
        (send (make-instance 'return-message :id (id request) :value values) connection))
    (error (e)
      (send (make-instance 'error-message :id (id request)
                                          :condition-type (type-of e)
                                          :condition-arguments (arguments e)
                                          :report (princ-to-string e))
            connection))))

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
                (handle (receive) connection)
              (reconnect ()
                :report (lambda (s) "Reconnect to ~a" (host connection))
                ;; FIXME: What to do if reconnection fails?
                (connect (host connection)))
              (continue ()
                :report "Ignore the message and continue processing."
                NIL))))))
