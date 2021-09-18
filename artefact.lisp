#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defclass registry ()
  ())

(defgeneric notice-artefact (file registry))
(defgeneric probe-artefact (file registry))
(defgeneric local-file (file registry))

(defclass artefact ()
  (id
   path))

(defmethod local-file (path (artefact artefact))
  (merge-pathnames path (path artefact)))

(defclass parent-artefact (artefact)
  ((children :initform () :accessor children)))

(defclass child-artefact (artefact)
  ())

(defclass machine ()
  (id
   registry))

(defclass client ()
  (machine
   connection
   (request-table :initform (make-hash-table :test 'eql))
   (timeout-list :initform '((0 NIL NIL)))))

(defclass server ()
  (machine
   clients))

(defmethod communication:handle ((message communication:message) (client client))
  (let ((handler (gethash (id message) (response-table client))))
    (when handler
      (remhash (id message) (response-table client))
      ;; FIXME: This could be done more efficiently...
      (setf (timeout-list client) (remove (id message) (timeout-list client) :key #'second))
      (funcall handler message)
      :ok)))

(defmethod communication:handle ((time integer) (client client))
  (let ((list (rest (timeout-list client))))
    (unwind-protect
         (loop for (timeout id handler) = (car list)
               do (when (or (null timeout) (< time timeout))
                    (return))
                  (remhash id (response-table client))
                  (setf list (rest list))
                  (funcall handler))
      (setf (rest (timeout-list client)) list))))

(defmethod send ((client client) (message communication:message) &key on-result (on-timeout :error) (timeout 10))
  (communication:send (connection client) message)
  (setf (gethash (id message) (response-table client)) (or on-result #'identity))
  (when on-timeout
    (let* ((timeout (+ timeout (get-universal-time)))
           (entry (list timeout (id message) (etypecase on-timeout
                                               ((eql :error) (lambda () (error 'request-failed :client client)))
                                               (function on-timeout)
                                               (symbol (fdefinition on-timeout))))))
      ;; Sorted insert into timeout list.
      (loop for cons on (timeout-list client)
            do (let ((time (caar cons)))
                 (when (< timeout time)
                   (setf (cdr cons) (cons (car cons) (cdr cons)))
                   (setf (car cons) entry)
                   (return)))
            finally (setf (cdr cons) entry))))
  message)

(defmacro with-result ((result client event-type &rest event-args) &body on-result)
  `(send ,client
         (make-instance ',event-type ,@event-args)
         :on-result (lambda (,result) ,@on-result)))

(defmacro with-eval ((client form) values-bind &body on-result)
  (let ((result (gensym "RESULT")))
    `(with-result (,result ,client 'communication:eval-request :form ,form)
       (etypecase ,result
         (communication:return-message
          (destructuring-bind (&optional ,@values-bind) (communication:value ,result)
            ,@on-result))
         (communication:error-message
          (error 'evaluation-failed
                 :client client
                 :condition-type (communication:condition-type ,result)
                 :arguments (communication:arguments ,result)
                 :report (communication:report ,result)))))))

(defun eval-on (client form)
  (with-eval (client form) (&rest _)
    (declare (ignore _))
    NIL))

(defmethod send ((client client) (path pathname) &key on-result on-timeout)
  (with-result (result client 'communication:file-transfer :path (local-file path client)
                                                      :payload (local-file path *server*))
    (etypecase result
      (communication:ok
       (funcall on-result result))
      (communication:error-message
       (error 'file-transfer-failed :path path :client client)))))
