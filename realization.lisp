#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defclass local-step ()
  ((predecessors :initarg :predecessors :initform () :accessor predecessors)
   (successors :initarg :successors :initform () :accessor successors)
   (complete-p :initarg :complete-p :initform NIL :accessor complete-p)
   (client :initarg :client :initform NIL :reader client)))

(defclass executor ()
  ())

(defgeneric realize (plan executor))
(defgeneric localize (operation component client))
(defgeneric map-steps (function plan &key gather))
(defgeneric client-suitable-p (client step))
(defgeneric select-client (step executor))
(defgeneric perform (plan executor))

(defmacro do-steps ((step plan &rest args) &body body)
  `(block NIL (map-steps (lambda (,step) ,@body) ,plan ,@args)))

(defmethod perform :around ((step local-step) (client client))
  (if (complete-p step)
      (promise:pend :success step)
      (promise:-> (call-next-method)
                  (:then () (setf (complete-p step) T) step))))

(defmethod realize ((plan plan) (executor executor))
  (map-steps (lambda (step) (realize step executor)) plan))

(defmethod realize ((step step) (executor executor))
  (localize (operation step) (component step) (select-client step executor)))

(defmethod map-steps (function (plan plan) &key gather)
  (let ((tentative (map 'list #'identity (first-steps plan)))
        (map (make-hash-table :test 'eq)))
    (loop while tentative
          for step = (pop tentative)
          do ())))

(defclass single-executor ()
  ((client :initarg :client :initform NIL :accessor client)))

(defmethod select-client ((step step) (executor single-executor))
  (client executor))

(defmethod realize ((plan plan) (executor single-executor))
  (unless (client executor)
    (dolist (client (list-clients *server*))
      (unless (eql :skip (do-steps (step plan)
                           (unless (client-suitable-p client step)
                             (return :skip))))
        (setf (client executor) client)
        (return))))
  (unless (client executor)
    (error "Could not find any client that can execute all steps in this plan."))
  (map-steps (lambda (step)
               (localize (operation step) (component step) (client executor)))
             plan :gather T))

(defun compute-step-sequence (plan)
  (let ((visit (make-hash-table :test 'eq))
        (sequence ()))
    (labels ((visit (step)
               (unless (gethash step visit)
                 (dolist (successor (successors step))
                   (visit successor))
                 (push step sequence)
                 (setf (gethash step visit) T))))
      (loop for step across (first-steps plan)
            do (visit step))
      sequence)))

(defmethod perform ((plan plan) (executor single-executor))
  (promise:do-promised (step (compute-step-sequence plan))
    (handler-bind ((error #'invoke-debugger))
      (perform step ()))))
