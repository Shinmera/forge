#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defvar *database*)

(defclass database ()
  ())

(defgeneric map-effects (function database &optional type parameters version))
(defgeneric list-effects (database &optional type parameters version))
(defgeneric find-effect (database type parameters version &optional error))
(defgeneric register-effect (database effect))

(defmethod list-effects ((database database) &optional type parameters version)
  (let ((effects ()))
    (map-effects (lambda (e) (push e effects)) database type parameters version)
    effects))

(defmethod find-effect ((database database) type parameters version &optional (error T))
  (map-effects (lambda (e) (return-from find-effect e)) database type parameters version)
  (when error
    (error "No ~s effect of version ~a in database matching~%  ~s"
           type (to-string version) parameters)))

(defmacro do-effects ((effect database &optional type parameters version) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block NIL
       (flet ((,thunk (,effect)
                ,@body))
         (map-effects #',thunk ,database ,type ,parameters ,version)))))

(defclass component (versioned-object)
  ())

(defgeneric supported-operations (component)
  (:method-combination append))

(defmethod supported-operations append ((component component))
  ())

(defmethod initialize-instance :after ((component component) &key)
  (dolist (op (supported-operations component))
    (make-effect op component)))

(defclass operation ()
  ())

(defgeneric dependencies (operation component)
  (:method-combination append))
(defgeneric perform (operation component client))
(defgeneric make-effect (operation component))
(defgeneric ensure-effect (operation component type parameters))

(defmethod dependencies append ((operation operation) (component component))
  ())

(defmethod make-effect ((operation symbol) (component component))
  (make-effect (prototype operation) component))

(defmethod ensure-effect (operation (component component) type parameters)
  (let* ((version (version component))
         (effect (or (find-effect *database* type parameters version NIL)
                     (register-effect *database* (make-instance type :parameters parameters :version version)))))
    (add-source operation component effect)
    effect))

(defclass dependency ()
  ((effect-type :initarg :effect-type :initform (support:arg! :effect-type) :reader effect-type)
   (parameters :initarg :parameters :initform (support:arg! :parameters) :reader parameters)
   (version :initarg :version :initform (parse-constraint T) :reader version)
   (hard :initarg :hard :initform T :reader hard-p)))

(defun depend (type parameters &key (version (parse-constraint T)) (hard T))
  (make-instance 'dependency
                 :effect-type type
                 :parameters parameters
                 :version version
                 :hard hard))

(defmethod print-object ((dependency dependency) stream)
  (print-unreadable-object (dependency stream :type T)
    (format stream "~s ~s ~s ~@[HARD~]"
            (effect-type dependency) (parameters dependency) (to-string (version dependency)) (hard-p dependency))))

(defclass effect (versioned-object)
  ((sources :initarg :sources :initform () :accessor sources)
   (parameters :reader parameters)))

(defgeneric sources (effect))
(defgeneric add-source (operation component effect))
(defgeneric parameters (effect))
(defgeneric normalize-parameters (effect parameters))
(defgeneric variant-p (effect-1 effect-2))

(defmethod initialize-instance ((effect effect) &key parameters)
  (call-next-method)
  (setf (slot-value effect 'parameters) (normalize-parameters effect parameters)))

(defmethod print-object ((effect effect) stream)
  (print-unreadable-object (effect stream :type T)
    (format stream "~s ~s" (type-of effect) (parameters effect))))

(defmethod add-source ((operation operation) (component component) (effect effect))
  (add-source (type-of operation) component effect))

(defmethod add-source ((operation symbol) (component component) (effect effect))
  (let ((source (list operation component)))
    (pushnew source (sources effect) :test #'equal)
    source))

(defmethod normalize-parameters ((effect symbol) parameters)
  (normalize-parameters (prototype effect) parameters))

(defmethod normalize-parameters ((effect effect) parameters)
  parameters)

(defmethod variant-p ((a effect) (b effect))
  (and (eq (type-of a) (type-of b))
       (equal (parameters a) (parameters b))))

(defclass policy ()
  ())

(defgeneric in-order-to (effect policy))
(defgeneric select-source (policy effect sources))
(defgeneric select-effect-set (policy sets))
(defgeneric compute-plan (effect policy))
(defgeneric make-operation (operation component effect policy))

(defmethod in-order-to ((effect effect) (policy policy))
  (select-source policy effect (sources effect)))

(defmethod make-operation ((operation symbol) (component component) (effect effect) (policy policy))
  (make-operation (make-instance operation) component effect policy))

(defmethod make-operation ((operation operation) (component component) (effect effect) (policy policy))
  operation)

(defclass executor ()
  ())

(defclass plan ()
  ((first-steps :initarg :first-steps :initform #() :reader first-steps)
   (final-steps :initarg :final-steps :initform #() :reader final-steps)))

(defgeneric make-step (operation component effect))

(defmethod make-step ((operation operation) (component component) (effect effect))
  (make-instance 'step
                 :operation operation
                 :component component
                 :effect effect))

(defclass step ()
  ((operation :initarg :operation :initform (support:arg! :operation) :reader operation)
   (component :initarg :component :initform (support:arg! :compoenent) :reader component)
   (effect :initarg :effect :initform (support:arg! :effect) :reader effect)
   (predecessors :initarg :predecessors :initform () :accessor predecessors)
   (successors :initarg :successors :initform () :accessor successors)
   (complete-p :initform NIL :accessor complete-p)))

(define-print-object-method* step
  "~s ~s" (type-of (operation step)) (type-of (component step)))

(defclass compound-step (step)
  ((inner-effect :initarg :inner-effect :initform (support:arg! :inner-effect) :reader inner-effect)))

(defgeneric execute (plan/step executor))
(defgeneric effect-realized-p (effect executor))
(defgeneric effect-needed-p (effect executor))
(defgeneric connect (from to))

(defmethod effect-realized-p ((effect effect) (executor executor))
  NIL)

(defmethod effect-needed-p ((effect effect) (executor executor))
  (or (not (effect-realized-p effect executor))
      (loop for predecessor in (predecessors effect)
            thereis (effect-needed-p effect executor))))

(defmethod connect ((from step) (to step))
  (pushnew to (successors from))
  (pushnew from (predecessors to)))

(defmethod disconnect ((from step) (to step))
  (setf (successors from) (delete to (successors from)))
  (setf (predecessors to) (delete from (predecessors to))))

;; FIXME: check compiler and compiler version congruity on compiler-operations.
;; FIXME: ensure 'same-system deps' are assigned to same client or same machine.
;; FIXME: actually cache stuff
;; FIXME: getting artefacts from one client to another
