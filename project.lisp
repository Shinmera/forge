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
  (register-effect effect)
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

(defgeneric list-effects (database &optional type parameters version))
(defgeneric find-effect (database type parameters version &optional (error T)))
(defgeneric register-effect (database effect))
(defgeneric list-sources (database effect))

(defclass component (versioned-object)
  ())

(defclass operation ()
  ())

(defclass effect (versioned-object)
  ())

(defclass policy ()
  ())

(defclass executor ()
  ())

(defclass plan ()
  (steps))

(defclass step ()
  (operation
   component
   predecessors
   successors))

(defgeneric ensure-effect (component operation type parameters))
(defgeneric in-order-to (effect policy))
(defgeneric select-source (policy effect sources))
(defgeneric perform (operation component))
(defgeneric compute-plan (effect policy))
(defgeneric execute (plan executor))
