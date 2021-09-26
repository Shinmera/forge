#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.client)

(defvar *forge-connection* NIL)

(support:define-condition* already-connected (error)
  (connection) ("Connection already established through~%  ~a" connection))

(support:define-condition* connection-lost (error)
  (connection) ("Connection through~%  ~a~%lost." connection))

(defun log (level message &rest args)
  (format (ecase level
            ((:info :error) *error-output*)
            ((:debug :trace) *debug-io*))
          "FORGE [~5a] ~?" level message args))

(defun try-connect (host name &key timeout)
  (support:handler-case* (communication:connect host name :timeout timeout)
    (error (e)
           (log :error "Failed to connect to~%  ~a~%~a" host e)
           NIL)))

(defun start (&key (address "127.0.0.1")
                   (port TCP:DEFAULT-PORT)
                   (timeout 1.0)
                   (name (machine-instance))
                   host
                   (if-unavailable :launch)
                   (launch-method :binary)
                   (launch-arguments ()))
  (when (connected-p)
    (error 'already-connected :connection *forge-connection*))
  (support:with-retry-restart ()
    (let ((host (or host (make-instance 'tcp:host :address address :port port))))
      (loop (with-simple-restart (retry "Retry connecting.")
              (let ((connection (or (try-connect host name :timeout timeout)
                                    (ecase if-unavailable
                                      ((NIL)
                                       NIL)
                                      (:error
                                       (error 'communication:connection-failed :host host :name name :report NIL))
                                      (:launch
                                       (setf host (apply #'launch-server launch-method :address address :port port launch-arguments))
                                       (invoke-restart 'retry))))))
                (setf *forge-connection* connection)
                (return connection)))))))

(defun stop ()
  (when (connected-p)
    (close *forge-connection*)
    (setf *forge-connection* NIL)))

(defun connected-p ()
  (and *forge-connection* (communication:alive-p *forge-connection*)))

(defun handle-reconnect (connection &key (on-reconnect-failure :sleep))
  (let ((host (communication:host connection)))
    (log :info "Trying to reconnect to~%  ~a" host)
    (with-simple-restart (abort "Exit reconnection.")
      (loop (setf connection (try-connect host (communication:name connection)))
            (when connection (return connection))
            (etypecase on-reconnect-failure
              (null (return))
              ((eql :sleep) (sleep 10))
              ((eql :error) (error 'connection-lost :connection connection))
              ((or symbol function)
               (with-simple-restart (reconnect "Attempt to reconnect again.")
                 (return (funcall on-reconnect-failure)))))))))

(defun command-loop (connection &key (on-disconnect :reconnect) (on-reconnect-failure :sleep))
  (with-simple-restart (communication:exit-command-loop "Exit the command loop.")
    (loop (unless (communication:alive-p connection)
            (restart-case
                (case on-disconnect
                  ((NIL)
                   (return NIL))
                  (:error
                   (error 'connection-lost :connection connection))
                  (:reconnect
                   (invoke-restart 'reconnect)))
              (reconnect ()
                :report "Attempt to reconnect."
                (setf connection (handle-reconnect connection :on-reconnect-failure on-reconnect-failure))
                (unless connection (return)))))
          (communication:handle1 connection))))

(defun dedicate (&rest start-args)
  (unless (connected-p)
    (apply #'start start-args))
  (command-loop *forge-connection*))

(defun forge-package-p (package)
  (flet ((match (name)
           (string= #1="ORG.SHIRAKUMO.FORGE" name :end2 (min (length name) (length #1#)))))
    (or (match (package-name package))
        (some #'match (package-nicknames package)))))

(defun prune-package (package)
  (do-symbols (symbol package (delete-package package))
    (when (eq (symbol-package symbol) package)
      (makunbound symbol)
      (fmakunbound symbol)
      (when (find-class symbol)
        (setf (find-class symbol) NIL)))))

(defun prune ()
  (kill-server)
  (prune-package (remove-if-not #'forge-package-p (list-all-packages))))

(defun request-effect (effect-type parameters &key (version T) (execute-on :self) (connection *forge-connection*))
  (communication:send! connection 'communication:effect-request
                       :effect-type effect-type
                       :parameters parameters
                       :version version
                       :execute-on execute-on))

(defun load-project (project &key (version T) (connection *forge-connection*))
  (request-effect (communication:make-dummy-symbol "ORG.SHIRAKUMO.FORGE.MODULES.LISP" "LOAD-EFFECT")
                  (list :project project) :version version :connection connection))
