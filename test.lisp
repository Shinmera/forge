#|
This file is a part of forge
(c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(setf *database* (make-instance 'basic-database))

(defclass lisp-file (component)
  ((depends-on :initarg :depends-on :initform () :reader depends-on)))
(defclass lisp-source-op (operation) ())
(defclass compile-op (lisp-source-op) ())
(defclass load-op (lisp-source-op) ())
(defclass load-fasl-op (operation) ())
(defclass compile-effect (effect) ())
(defclass load-effect (effect) ())

(defmethod supported-operations ((file lisp-file))
  '(compile-op load-op load-fasl-op))

(defmethod dependencies ((op lisp-source-op) (c lisp-file))
  (loop for component in (depends-on c)
        collect (depend 'load-effect component)))

(defmethod make-effect ((op compile-op) (c lisp-file))
  (ensure-effect op c 'compile-effect c))

(defmethod make-effect ((op load-op) (c lisp-file))
  (ensure-effect op c 'load-effect c))

(defmethod make-effect ((op load-fasl-op) (c lisp-file))
  (ensure-effect op c 'load-effect c))

(defmethod dependencies ((op load-fasl-op) (c lisp-file))
  (list (depend 'compile-effect c)))

(defun run-test ()
  (let* ((c0 (make-instance 'lisp-file))
         (c1 (make-instance 'lisp-file :depends-on (list c0))))
    (compute-plan (find-effect *database* 'load-effect c1 (parse-constraint T))
                  (make-instance 'basic-policy))))
