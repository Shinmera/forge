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

(defun load-server (&optional (forge-source-root *forge-source-root*))
  (unless (and (find-package '#:org.shirakumo.forge.server)
               (find-symbol '#:loaded-p '#:org.shirakumo.forge.server)
               (symbol-value (find-symbol '#:loaded-p '#:org.shirakumo.forge.server)))
    (load (support:try-files (merge-pathnames "server/load.fasl" forge-source-root)
                             (merge-pathnames "server/load.lisp" forge-source-root)
                             (merge-pathnames "../server/load.fasl" #.*load-pathname*)))))

(defgeneric launch-server (method &key connect &allow-other-keys))

(defmethod launch-server ((method (eql :binary)) &key (binary *forge-binary*) (address "127.0.0.1") (port TCP:DEFAULT-PORT) connect)
  (when (and *forge-process* (null (support:exit-code *forge-process*)))
    (error 'process-already-running :process *forge-process*))
  (setf *forge-process* (support:launch *forge-binary* (list "launch" address port)))
  (let ((host (make-instance 'tcp:host :address address :port port)))
    (if connect
        (communication:connect host :timeout 1.0)
        host)))

(defmethod launch-server ((method (eql :launch-self)) &key)
  ;; TODO: self-launching
  )

(defmethod launch-server ((method (eql :in-process)) &key connect)
  (declare (ignore connect))
  #+asdf (if (asdf:find-system :forge-server)
             (asdf:load-system :forge-server)
             (load-server))
  #-asdf (load-server)
  (communication:connect (communication:serve (make-instance 'in-process:host))))

(defun start (&key (address "127.0.0.1")
                   (port TCP:DEFAULT-PORT)
                   (timeout 1.0)
                   host
                   (if-unavailable :launch)
                   (launch-method :binary)
                   (launch-arguments ()))
  (when (connected-p)
    (error 'already-connected :connection *forge-connection*))
  (support:with-retry-restart ()
    (or (communication:connect (or host (make-instance 'tcp:host :address address :port port)) :timeout timeout)
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
    (communication:exit *forge-connection*)
    (setf *forge-connection* NIL)))

(defun connected-p ()
  (and *forge-connection* (communication:alive-p *forge-connection*)))

(defun dedicate (&rest start-args)
  (unless (connected-p)
    (apply #'start start-args))
  (communication:command-loop *forge-connection*))
