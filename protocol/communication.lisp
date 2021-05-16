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
