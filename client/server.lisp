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
(defvar *forge-source-root*
  (let ((this #.(or *compile-file-pathname* *load-pathname*)))
    (make-pathname :name NIL :type NIL :version NIL
                   :directory (butlast (pathname-directory this))
                   :host (pathname-host this)
                   :device (pathname-device this))))

(defun load-server (&optional (forge-source-root *forge-source-root*))
  (unless (and (find-package '#:org.shirakumo.forge)
               (find-symbol '#:loaded-p '#:org.shirakumo.forge)
               (symbol-value (find-symbol '#:loaded-p '#:org.shirakumo.forge)))
    (load (support:try-files (merge-pathnames "bootstrap.fasl" forge-source-root)
                             (merge-pathnames "bootstrap.lisp" forge-source-root)
                             (merge-pathnames "bootstrap.fasl" #.*load-pathname*)
                             (merge-pathnames "bootstrap.lisp" #.*load-pathname*)))))

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

(defun kill-server ()
  (stop)
  (when *forge-process*
    (support:terminate *forge-process*)
    (setf *forge-process* NIL)))
