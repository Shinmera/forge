#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

#|
Let's imagine we have a component c1, the following operations, and
the following effects resulting from performing the operations:

  (load c1)           => e[load, c1]
  (compile c1)        => e[compile, c1]
  (load-compiled c1)  => e[load, c1]

In this case the same effect (e[load, c1]) can be achieved in two ways:

1. (load c1)
2. (load-compiled c1)

But (load-compiled c1) depends on e[compile, c1], which can only be
achieved by performing (compile c1). In any case, the point is that
we have a situation where the same effect can be achieved by different
means.

When computing a plan, we thus need a way to select which of these
variants to use, probably by way of a policy object. (This policy
object could then also establish extra parameters for operations
in the plan.)

In order to facilitate this selection process as well as the version
constraint resolution, we need to establish a way for operation/
component pairs to share the effects (by identity), for the planner
to perform an inverse search of finding operation/component pairs
that would facilitate an effect, and for operation/component pairs
to loosely specify dependencies on effects with version constraints
without having to name an effect by identity:

  (ensure-effect
    source-component       ; The component and operation pair that
    source-operation       ; produce this effect as a result.
    effect-type            ; The type of effect produced.
    effect-parameters)     ; Parameters that uniquely identify the
                           ; effect within the database.
    => effect
  
  (in-order-to
    effect                 ; The effect we're trying to produce.
    policy)                ; A way to disambiguate multiple sources.
    => (operation component) 

  (list-effects
    effect-type            ; The type of effect we require.
    effect-parameters      ; Parameters of the effect that uniquely
                           ; identify it.
    version-constraint)    ; A parameter to further constrain the
                           ; versions permitted for the effect.
    => [effect]

We can expand the protocol with the following lower-level functions
to use as a base for implementing the above:

  (list-effects &optional effect-type effect-parameters version-constraint)
    => [effect]
  (find-effect effect-type effect-parameters version &optional error)
    => effect
  (register-effect effect source-component source-operation)
    => effect
  (list-sources effect)
    => [(operation component)]
  (select-source policy effect sources)
    => (operation component)

The krux in this interface is the implementation of these "effect
parameters" that uniquely identify effects of the same type. It
seems to me that this is going to be highly domain specific. In
most cases however it's likely going to just be a tuple of the
originating project and a "path" identifier within the project
that ties the effect to the component in some fashion.

However it is implemented, this interface is highly verbose and
there must be a way for this to be automatically handled depending
on the project type. I'll have to think more on how exactly to
do that, though.
|#

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

(defgeneric supported-operations (component))

(defmethod initialize-instance :after ((component component) &key)
  (dolist (op (supported-operations component))
    (make-effect op component)))

(defclass operation ()
  ())

(defgeneric dependencies (operation component))
(defgeneric perform (operation component))
(defgeneric make-effect (operation component))
(defgeneric ensure-effect (operation component type parameters))

(defmethod make-effect ((operation symbol) (component component))
  (make-effect (c2mop:class-prototype (find-class operation)) component))

(defmethod ensure-effect (operation (component component) type parameters)
  (let ((effect (or (find-effect *database* type parameters (version component) NIL)
                    (register-effect *database* (make-instance type :parameters parameters :version (version component))))))
    (add-source component operation effect)
    effect))

(defclass dependency ()
  ((effect-type :initarg :effect-type :reader effect-type)
   (parameters :initarg :parameters :reader parameters)
   (version :initarg :version :initform (parse-constraint T) :reader version)
   (hard :initarg :hard :initform T :reader hard-p)))

(defun depend (type parameters &key (version (parse-constraint T)) (hard T))
  (make-instance 'dependency
                 :effect-type type
                 :parameters parameters
                 :version version
                 :hard hard))

(defclass effect (versioned-object)
  ((sources :initarg :sources :accessor sources)
   (parameters :reader parameters)))

(defgeneric sources (effect))
(defgeneric add-source (operation component effect))
(defgeneric parameters (effect))
(defgeneric normalize-parameters (effect parameters))

(defmethod initialize-instance ((effect effect) &key parameters)
  (call-next-method)
  (setf (slot-value effect 'parameters) (normalize-parameters effect parameters)))

(defmethod add-source ((operation operation) (component component) (effect effect))
  (add-source (type-of operation) component effect))

(defmethod add-source ((operation symbol) (component component) (effect effect))
  (let ((source (list operation component)))
    (pushnew source (sources effect) :test #'equal)
    source))

(defmethod normalize-parameters ((effect symbol) parameters)
  (normalize-parameters (c2mop:class-prototype (find-class effect)) parameters))

(defmethod normalize-parameters ((effect effect) parameters)
  parameters)

(defclass policy ()
  ())

(defgeneric in-order-to (effect policy))
(defgeneric select-source (policy effect sources))
(defgeneric compute-plan (effect policy))
(defgeneric make-operation (operation component effect policy))

(defmethod in-order-to ((effect effect) (policy policy))
  (select-source policy effect (sources effect)))

(defclass executor ()
  ())

(defclass plan ()
  ((first-steps :initarg :first-steps :initform #() :reader first-steps)
   (final-steps :initarg :final-steps :initofrm #() :reader final-steps)))

(defgeneric make-step (plan operation component effect))

(defclass step ()
  ((operation :initarg :operation :reader operation)
   (component :initarg :component :reader component)
   (effect :initarg :effect :reader effect)
   (predecessors :initarg :predecessors :initform () :accessor predecessors)
   (successors :initarg :successors :initform () :accessor successors)))

(defgeneric execute (plan/step executor))
(defgeneric effect-realized-p (effect executor))
(defgeneric connect (from to))
