#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defclass basic-database (database)
  ((effects :initform (make-hash-table :test 'eq) :reader effects)))

(defmethod map-effects (function (database basic-database) &optional type parameters version)
  (let ((function (etypecase function
                    (function function)
                    (symbol (fdefinition function)))))
    (cond (version
           (let ((tables (gethash type (effects database))))
             (when tables
               (let ((effects (gethash (normalize-parameters type parameters) tables)))
                 (when effects
                   (etypecase version
                     (version-constraint
                      (loop for effect being the hash-values of effects
                            do (when (version-match-p (version effect) version)
                                 (funcall function effect))))
                     (version
                      (let ((effect (gethash (to-string version) effects)))
                        (when effect (funcall function effect))))))))))
          (parameters
           (let ((tables (gethash type (effects database))))
             (when tables
               (let ((effects (gethash (normalize-parameters type parameters) tables)))
                 (when effects
                   (loop for effect being the hash-values of effects
                         do (funcall function effect)))))))
          (type
           (let ((tables (gethash type (effects database))))
             (when tables
               (loop for effects being the hash-values of tables
                     do (loop for effect being the hash-values of effects
                              do (funcall function effect))))))
          (T
           (loop for tables being the hash-values of (effects database)
                 do (loop for effects being the hash-values of tables
                          do (loop for effect being the hash-values of effects
                                   do (funcall function effect))))))))

(defmethod register-effect ((database basic-database) (effect effect))
  (let* ((tables (or (gethash (type-of effect) (effects database))
                     (setf (gethash (type-of effect) (effects database))
                           (make-hash-table :test 'equal))))
         (effects (or (gethash (parameters effect) tables)
                      (setf (gethash (parameters effect) tables)
                            (make-hash-table :test 'equal))))
         (version (to-string (version effect)))
         (existing (gethash version effects)))
    (when (and existing (not (eq existing effect)))
      (error "Different effect with same parameters and version already exists!"))
    (setf (gethash version effects) effect)))

(defclass parameter-plist-effect (effect)
  ())

(defmethod normalize-parameters ((effect parameter-plist-effect) parameters)
  (let ((alist (loop for (k v) on parameters by #'cddr
                     collect (cons k v))))
    (setf alist (sort alist (lambda (a b)
                              (if (string= (car a) (car b))
                                  (support:generic< (cdr a) (cdr b))
                                  (support:generic< (car a) (car b))))))
    (loop for (k . v) in alist
          collect k collect v)))

(defclass parameter-alist-effect (effect)
  ())

(defmethod normalize-parameters ((effect parameter-alist-effect) parameters)
  (sort parameters (lambda (a b)
                     (if (string= (car a) (car b))
                         (support:generic< (cdr a) (cdr b))
                         (support:generic< (car a) (car b))))))

(defclass naive-policy (policy)
  ())

(defmethod select-source ((policy naive-policy) (effect effect) sources)
  (first sources))

(defclass basic-policy (policy)
  ())

(defmethod compute-plan ((effect effect) (policy basic-policy))
  (let ((visit (make-hash-table :test 'eq)))
    ))

(defclass basic-executor (executor)
  ((force :initarg :force :initform NIL :accessor force)))

(defmethod execute ((step step) (executor basic-executor))
  (when (or (force executor)
            (not (effect-realized-p (effect step) executor)))
    (perform (operation step) (component step))))

(defmethod execute ((plan plan) (executor basic-executor))
  (loop for step across (steps plan)
        do (execute step executor)))

