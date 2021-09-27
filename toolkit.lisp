#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defun prototype (thing)
  (etypecase thing
    (symbol
     (c2mop:class-prototype (c2mop:ensure-finalized (find-class thing))))
    (class
     (c2mop:class-prototype (c2mop:ensure-finalized thing)))
    (object
     thing)))

(defun pophash (key table &optional default)
  (let ((value (gethash key table default)))
    (remhash key table)
    value))

(defun wait-for-thread-exit (thread)
  (loop for i from 0
        do (unless (bt:thread-alive-p thread)
             (return))
           (when (<= 10 i)
             (restart-case (error "Message thread is not shutting down!")
               (interrupt (&optional (function #'break))
                 :report "Try to interrupt the thread."
                 (bt:interrupt-thread thread function))
               (abort ()
                 :report "Kill and forget the thread."
                 (bt:destroy-thread thread))
               (continue ()
                 :report "Continue waiting.")))
           (sleep 0.1)))

(defmacro with-event-loop (bindings &body body)
  (let ((last-check (gensym "LAST-CHECK")))
    `(let ((,last-check (get-internal-real-time))
           ,@bindings)
       (loop ,@body
             ;; Backoff to make sure we don't overheat
             (let* ((new-time (get-internal-real-time))
                    (seconds-passed (/ (- new-time ,last-check) internal-time-units-per-second)))
               (when (< seconds-passed 0.01)
                 (sleep (- 0.01 seconds-passed)))
               (setf ,last-check new-time))))))
