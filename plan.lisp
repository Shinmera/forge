#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

;;;; Description:
;;; This file defines the general planning protocol and algorithm.
;;; The planning system is responsible for taking abstract definitions of
;;; operations, components, effects, and versions of each of them and
;;; computing a tree (plan) of required operations on components to achieve
;;; a desired end-effect.

(in-package #:org.shirakumo.forge)

(support:define-condition* dependency-cycle-detected (error)
  (effect) ("Dependency cycle detected around~%  ~a" effect))

(support:define-condition* unsatisfiable-dependency (warning)
  (dependency operation component) ("Cannot find any effects that satisfy~%  ~a~%of~%  ~a ~a" dependency operation component))

(support:define-condition* unsatisfiable-effect (error)
  (effect) ("Cannot compute a plan to reach~%  ~a~%no possible solutions found to resolve all constraints!" effect))

(defvar *database*)

(defclass database ()
  ())

(defgeneric map-effects (function database &optional type parameters version))
(defgeneric list-effects (database &optional type parameters version))
(defgeneric find-effect (database type parameters version &optional error))
(defgeneric register-effect (database effect))
(defgeneric achieved-version (database effect))
(defgeneric (setf achieved-version) (version database effect))

(defmethod map-effects (function (database (eql T)) &optional type parameters version)
  (map-effects function *database* type parameters version))

(defmethod list-effects ((database (eql T)) &optional type parameters version)
  (list-effects *database* type parameters version))

(defmethod list-effects ((database database) &optional type parameters version)
  (let ((effects ()))
    (map-effects (lambda (e) (push e effects)) database type parameters version)
    effects))

(defmethod find-effect ((database (eql T)) type parameters version &optional (error T))
  (find-effect *database* type parameters version error))

(defmethod find-effect ((database database) type parameters version &optional (error T))
  (map-effects (lambda (e) (return-from find-effect e)) database type parameters version)
  (when error
    (error "No ~s effect of version ~a in database matching~%  ~s"
           type (to-string version) parameters)))

(defmethod achieved-version ((database (eql T)) effect)
  (achieved-version *database* effect))

(defmacro do-effects ((effect database &optional type parameters version) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block NIL
       (flet ((,thunk (,effect)
                ,@body))
         (map-effects #',thunk ,database ,type ,parameters ,version)))))

(defclass component (versioned-object)
  ((name :initarg :name :initform (support:arg! :name) :reader name)))

(define-print-object-method* component
  "~s ~a" name (to-string (version component)))

(defgeneric supported-operations (component)
  (:method-combination append))

(defmethod supported-operations append ((component component))
  ())

(defmethod shared-initialize :after ((component component) slots &key)
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
    (format stream "~s ~s ~a ~@[HARD~]"
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
    (format stream "~s" (parameters effect))))

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

(defmethod achieved-version ((database database) (effect effect))
  *unknown-version*)

(defmethod (setf achieved-version) ((current (eql T)) (database database) (effect effect))
  (setf (achieved-version database effect) (version effect)))

(defclass compiler (versioned-object)
  ((name :initarg :name :initform (support:arg! :name) :reader name)
   (cache-directory :initarg :cache-directory :initform NIL :accessor cache-directory)))

(defmethod initialize-instance :after ((compiler compiler) &key)
  (unless (cache-directory compiler)
    (setf (cache-directory compiler) (remove-if #'unsafe-path-char-p
                                                (format NIL "~(~a-~a~)"
                                                        (name compiler)
                                                        (to-string (version compiler)))))))

(defclass policy ()
  ((compiler :initarg :compiler :initform NIL :accessor compiler)))

(defgeneric in-order-to (effect policy))
(defgeneric select-source (policy effect sources))
(defgeneric select-effect-set (policy sets))
(defgeneric compute-plan (effect policy))
(defgeneric make-operation (operation policy))
(defgeneric select-compiler (effect policy))

(defmethod in-order-to ((effect effect) (policy policy))
  (select-source policy effect (sources effect)))

(defmethod make-operation ((operation symbol) (policy policy))
  (make-operation (make-instance operation) policy))

(defmethod make-operation ((operation operation) (policy policy))
  operation)

(defmethod compute-plan :before ((effect effect) (policy policy))
  (unless (compiler policy)
    (setf (compiler policy) (select-compiler effect policy))))

(defclass plan ()
  ((first-steps :initarg :first-steps :initform #() :reader first-steps)
   (final-steps :initarg :final-steps :initform #() :reader final-steps)))

(defgeneric make-step (operation component effect))
(defgeneric effect-achieved-p (effect policy))

(defmethod effect-achieved-p ((effect effect) (policy policy))
  ;;                                 v This feels wrong
  (version= effect (achieved-version T effect)))

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
   (successors :initarg :successors :initform () :accessor successors)))

(define-print-object-method* step
  "~s ~s" (type-of (operation step)) (type-of (component step)))

(defclass compound-step (step)
  ((inner-effect :initarg :inner-effect :initform (support:arg! :inner-effect) :reader inner-effect)))

(defgeneric connect (from to))

(defmethod connect ((from step) (to step))
  (pushnew to (successors from))
  (pushnew from (predecessors to)))

(defmethod disconnect ((from step) (to step))
  (setf (successors from) (delete to (successors from)))
  (setf (predecessors to) (delete from (predecessors to))))

;; FIXME: way of declaring "latest version" of known set
;;;       WORKAROUND: always pick latest version if multiple possible.
