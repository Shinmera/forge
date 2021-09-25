#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge.communication
  (:use #:cl)
  (:local-nicknames
   (#:support #:org.shirakumo.forge.support))
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
   #:send!
   #:reply!
   #:client-connection
   #:server-connection
   #:connections
   #:message
   #:id
   #:reply
   #:connection-lost
   #:command
   #:exit
   #:ok
   #:ping
   #:pong
   #:connect
   #:name
   #:error-message
   #:warning-message
   #:esend
   #:condition-type
   #:arguments
   #:report
   #:eval-request
   #:form
   #:return-message
   #:value
   #:effect-request
   #:effect-type
   #:parameters
   #:version
   #:execute-on
   #:artefact
   #:artefact-source
   #:artefact-target
   #:dummy-symbol
   #:make-dummy-symbol
   #:dummy-symbol-package
   #:dummy-symbol-name
   #:encode-message
   #:decode-message
   #:handle1
   #:exit-command-loop
   #:handshake)
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
