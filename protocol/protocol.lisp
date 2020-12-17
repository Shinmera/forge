#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge.protocol
  (:use #:cl)
  (:export))

(in-package #:org.shirakumo.forge.protocol)

(defclass host () ())
(defgeneric open (host)) ; => CONNECTION
(defgeneric serve (host)) ; => CONNECTION

(defclass connection () ())
(defgeneric host (connection)) ; => HOST
(defgeneric alive-p (connection)) ; => BOOLEAN
(defgeneric close (connection)) ; => CONNECTION
(defgeneric write (message connection))
(defgeneric read (connection &key timeout)) ; => MESSAGE

(defclass client-connection (connection) ())
(defclass server-connection (connection) ())

(defgeneric connections (server-connection)) ; => (CONNECTION)
(defmethod write (message (server server-connection))
  (dolist (connection (connections server))
    (write message connection)))

(defclass message () ())
(defclass connection-established (message)
  ((connection)))
(defclass connection-lost (message)
  ((connection)))

(defgeneric encode-message (message stream))
(defgeneric decode-message (type stream))
