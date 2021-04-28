#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.protocol)

(defvar *timeout* NIL)

(defmacro with-timeout (timeout &body body)
  `(let ((*timeout* ,timeout))
     ,@body))

(defclass host () ())
(defgeneric connect (host &key timeout)) ; => CONNECTION
(defgeneric serve (host &key timeout)) ; => CONNECTION

(defclass connection () ())
(defgeneric host (connection)) ; => HOST
(defgeneric alive-p (connection)) ; => BOOLEAN
(defgeneric send (message connection))
(defgeneric receive (connection &key timeout)) ; => MESSAGE | NIL

(defclass client-connection (connection) ())
(defclass server-connection (connection) ())

(defgeneric connections (server-connection)) ; => (CONNECTION)
(defmethod send (message (server server-connection))
  (dolist (connection (connections server))
    (write message connection)))

(defclass message () ())
(defclass connection-established (message) ())
(defclass connection-lost (message) ())
(defclass command (message) ())

(defgeneric execute (command))

(defmethod send ((message command) (connection connection))
  (call-next-method)
  (read connection))

(defgeneric encode-message (message stream))
(defgeneric decode-message (type stream))
