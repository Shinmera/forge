#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge.protocol
  (:use #:cl)
  ;; communication.lisp
  (:export
   #:*timeout*
   #:with-timeout
   #:host
   #:connect
   #:serve
   #:connection
   #:host
   #:alive-p
   #:send
   #:receive
   #:handle
   #:client-connection
   #:server-connection
   #:connections
   #:message
   #:connection-established
   #:connection-lost
   #:command
   #:exit
   #:encode-message
   #:decode-message
   #:command-loop
   #:exit-command-loop)
  ;; artefact.lisp
  (:export
   #:artefact)
  ;; binary.lisp
  (:export
   #:wu8
   #:wu16
   #:wu32
   #:wu64
   #:ru8
   #:ru16
   #:ru32
   #:ru64
   #:ri8
   #:ri16
   #:ri32
   #:ri64
   #:ensure-encoding-type-id
   #:encoding-type-id
   #:define-encoding))
