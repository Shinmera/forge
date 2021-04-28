#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.client)

(defvar *forge-binary*
  #+unix (merge-pathnames ".local/bin/forge" (user-homedir-pathname))
  #+win32 "forge.exe")
(defvar *forge-process* NIL)
(defvar *forge-connection* NIL)
(defvar *forge-source-root*
  (let ((this #.(or *compile-file-pathname* *load-pathname*)))
    (make-pathname :name NIL :type NIL :version NIL
                   :directory (butlast (pathname-directory this))
                   :host (pathname-host this)
                   :device (pathname-device this))))

(defmacro with-retry-restart ((&optional (name 'retry) (format-string "Retry") &rest format-args) &body body)
  (let ((block (gensym "BLOCK"))
        (retry (gensym "RETRY")))
    `(block ,block
       (tagbody
          ,retry
          (restart-case
              (return-from ,block ,@body)
            (,name ()
              :report (lambda (s) (format s ,format-string ,@format-args))
              (go ,retry)))))))

(defun load-server (&optional (forge-source-root *forge-source-root*))
  (load (merge-pathnames "server/load.lisp" forge-source-root)))

(defgeneric launch-server (method &key connect &allow-other-keys))

(defmethod launch-server ((method (eql :binary)) &key (binary *forge-binary*) (address "127.0.0.1") (port TCP:DEFAULT-PORT) connect)
  (when (and *forge-process* (null (support:exit-code *forge-process*)))
    (error 'process-already-running :process *forge-process*))
  (setf *forge-process* (support:launch *forge-binary* (list "launch" address port)))
  (let ((host (make-instance 'tcp:host :address address :port port)))
    (if connect
        (protocol:connect host :timeout 1.0)
        host)))

(defmethod launch-server ((method (eql :fork)) &key)
  ;; TODO: self-forking
  )

(defmethod launch-server ((method (eql :launch-self)) &key)
  ;; TODO: self-launching
  )

(defmethod launch-server ((method (eql :in-process)) &key connect)
  (declare (ignore connect))
  #+asdf (if (asdf:find-system :forge-server)
             (asdf:load-system :forge-server)
             (load-server))
  #-asdf (load-server)
  (protocol:connect (protocol:serve (make-instance 'in-process:host))))

(defun start (&key (address "127.0.0.1")
                   (port TCP:DEFAULT-PORT)
                   (timeout 1.0)
                   (if-unavailable :launch)
                   (launch-method :binary)
                   (launch-arguments ()))
  (when (connected-p)
    (error 'already-connected :connection *forge-connection*))
  (with-retry-restart ()
    (or (protocol:connect (make-instance 'tcp:host :address address :port port) :timeout timeout)
        (ecase if-unavailable
          ((NIL)
           NIL)
          (:error
           (error 'connection-failed :address address))
          (:launch
           (apply #'launch-server launch-method
                  :address address :port port :connect T
                  launch-arguments))))))

(defun stop ()
  (when (connected-p)
    (protocol:exit *forge-connection*)
    (setf *forge-connection* NIL)))

(defun connected-p ()
  (and *forge-connection* (protocol:alive-p *forge-connection*)))

(defun dedicate (&rest start-args)
  (unless (connected-p)
    (apply #'start start-args))
  (protocol:command-loop *forge-connection*))
