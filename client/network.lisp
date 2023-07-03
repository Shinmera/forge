(in-package #:org.shirakumo.forge.client)

(defvar *connection* NIL)
(defvar *machine* (machine-instance))

(support:define-condition* already-connected (error)
  (connection) ("Connection already established through~%  ~a" connection))

(support:define-condition* connection-lost (error)
  (connection) ("Connection through~%  ~a~%lost." connection))

(defun log (level message &rest args)
  (format (ecase level
            ((:info :error) *error-output*)
            ((:debug :trace) *debug-io*))
          "FORGE [~5a] ~?" level message args))

(defun try-connect (host machine &key id timeout)
  (support:handler-case* (communication:connect host machine :id id :timeout timeout)
    (error (e)
           (log :error "Failed to connect to~%  ~a~%~a" host e)
           NIL)))

(defun connected-p (&optional (connection *connection*))
  (and connection (communication:alive-p connection)))

(defun start (&key (address "127.0.0.1")
                   (port TCP:DEFAULT-PORT)
                   (timeout 1.0)
                   (machine *machine*)
                   id
                   host
                   (if-unavailable :launch)
                   (launch-method :binary)
                   (launch-arguments ())
                   (dedicate T))
  (when (connected-p)
    (error 'already-connected :connection *connection*))
  (support:with-retry-restart ()
    (communication:init-id-counter machine)
    (setf *machine* machine)
    (let ((host (or host (make-instance 'tcp:host :address address :port port))))
      (loop (with-simple-restart (retry "Retry connecting.")
              (let ((connection (or (try-connect host machine :id id :timeout timeout)
                                    (ecase if-unavailable
                                      ((NIL)
                                       NIL)
                                      (:error
                                       (error 'communication:connection-failed :host host :report NIL))
                                      (:launch
                                       (setf host (apply #'launch-server launch-method :address address :port port launch-arguments))
                                       (invoke-restart 'retry))))))
                (setf *connection* connection)
                (if dedicate
                    (return
                      (unwind-protect (command-loop connection)
                        (ignore-errors (close connection))
                        (setf *connection* NIL)))
                    (return connection))))))))

(defun stop (&optional (connection *connection*))
  (cond ((find-restart 'communication:exit-command-loop)
         (invoke-restart 'communication:exit-command-loop))
        ((connected-p connection)
         (ignore-errors (communication:send! connection 'communication:exit))
         (ignore-errors (close connection))
         (when (eq connection *connection*) (setf *connection* NIL)))))

(defun handle-reconnect (connection &key (on-reconnect-failure :sleep))
  (let ((host (communication:host connection)))
    (log :info "Trying to reconnect to~%  ~a" host)
    (with-simple-restart (abort "Exit reconnection.")
      (loop (setf connection (try-connect host *machine*))
            (when connection (return connection))
            (etypecase on-reconnect-failure
              (null (return))
              ((eql :sleep) (sleep 10))
              ((eql :error) (error 'connection-lost :connection connection))
              ((or symbol function)
               (with-simple-restart (reconnect "Attempt to reconnect again.")
                 (return (funcall on-reconnect-failure)))))))))

(defun command-loop (connection &key (on-disconnect :reconnect) (on-reconnect-failure :sleep) until)
  (restart-case
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
            (handler-case
                (let ((message (communication:receive connection :timeout 1.0)))
                  (when message
                    (handler-case
                        (communication:handle message connection)
                      (error (e)
                        (communication:esend connection e message)))
                    (when (and until (equal (communication:id message) until))
                      (invoke-restart 'quit))))
              (error (e)
                (ignore-errors (communication:esend connection e)))))
    (communication:exit-command-loop ()
      :report "Close the connection and exit the command loop."
      (close connection))
    (quit ()
      :report "Quit the command loop without closing the connection"
      connection)))
