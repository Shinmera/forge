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

(defclass basic-policy (policy)
  ())

(defmethod select-source ((policy basic-policy) (effect effect) sources)
  (first sources))

(defun unify-dependency-sets (a b)
  (flet ((try-unify (a b)
           (let ((table (make-hash-table :test 'eq)))
             (dolist (b-effect b)
               (setf (gethash b-effect table) T))
             (dolist (a-effect a)
               (setf (gethash a-effect table) T)
               (dolist (b-effect b)
                 (cond ((eq a-effect b-effect)
                        (return))
                       ((variant-p a-effect b-effect)
                        (return-from try-unify ())))))
             (loop for k being the hash-keys of table
                   collect k))))
    (cond ((null a) ())
          ((null b) ())
          ((eql T a) b)
          ((eql T b) a)
          (T
           (let ((set ()))
             (dolist (choice a set)
               (dolist (candidate b)
                 (let ((result (try-unify choice candidate)))
                   (when result (push result set))))))))))

(defmethod compute-plan ((effect effect) (policy basic-policy))
  (let ((visit (make-hash-table :test 'equal)))
    ;; Note: We first compute the set of possible version constraints for all involved effects. If this set turns
    ;;       out to be empty for the root effect, we know it's unsatisfiable overall. Most likely though we are
    ;;       going to get a huge set of possible alternatives to pick from. We actually resolve this set in a second
    ;;       step where we compute the actual plan.
    (labels ((visit (effect)
               (etypecase (gethash effect visit :none)
                 ((eql :tentative) (error "Cycle."))
                 (list (gethash effect visit :none))
                 ((eql :none)
                  (setf (gethash effect visit) :tentative)
                  ;; Note: We eagerly select the source here, which can lead to an unsatisfiable plan even when
                  ;;       another source might provide a satisfiable plan. However, for a first attempt, and for
                  ;;       my own sanity's sake, I'm going to ignore this for the time being.
                  (let* ((source (select-source policy effect (sources effect)))
                         (component (second source))
                         (operation (make-operation (first source) component effect policy))
                         ;; Note: We only consider hard dependencies here. Optionals are pulled in in another phase
                         ;;       once we have already settled on a version set for everything else. This means we
                         ;;       potentially don't select optionals that might be possible in another set, but it
                         ;;       massively simplifies the plan computation, and at this point I'm too tired of trying
                         ;;       to come up with a holistic solution to continue doing the "absolutely right thing".
                         (dependencies (remove-if-not #'hard-p (dependencies operation component)))
                         (depchoices T))
                    (dolist (dependency dependencies)
                      (let ((choices ()))
                        (do-effects (effect *database* (effect-type dependency) (parameters dependency) (version dependency))
                          (dolist (choice (visit effect))
                            (push choice choices)))
                        ;; This is where the combinatorial explosion happens
                        (setf depchoices (unify-dependency-sets depchoices choices))))
                    (setf (gethash effect visit)
                          (if (eql T depchoices) (list (list effect))
                              (loop for choice in depchoices
                                    collect (list* effect choice)))))))))
      (visit effect))))

(defclass basic-executor (executor)
  ((force :initarg :force :initform NIL :accessor force)))

(defmethod execute ((step step) (executor basic-executor))
  (when (or (force executor)
            (not (effect-realized-p (effect step) executor)))
    (perform (operation step) (component step))))

(defmethod execute ((plan plan) (executor basic-executor))
  (loop for step across (steps plan)
        do (execute step executor)))


#|
Determining whether there is a valid constellation of versions is
equivalent to determining if a system of implications has a solution.

Our job is to figure out which of these implications to uphold in order
for the system to become valid. Once we have reduced it to a set of
satisfiable constraints, we can then traverse the tree and eagerly
select from the sources that match the constraints.

Since a source is equivalent to a singular 'thing', we opt to simplify
the illustration here by bundling each source into a singular entity
called 'c'. Each of these entities is then modelled as an integer
that can be constrained, and whose discrete states each have an
associated implication.

# Single solution problem
| c0 = 1 ⟹ c1 = 1 & c2 = 1
| c1 = 1 ⟹ c2 = 1
| c1 = 2 ⟹ c2 = 2
| c2 = 1 ⟹ c3 = 1
| c2 = 2 ⟹ c3 = 2
| c3 = 1 ⟹ T
| c3 = 2 ⟹ T
⟹ c0 = 1 & c1 = 1 & c2 = 1 & c3 = 1

# Multiple solution problem
| c0 = 1 ⟹ c1 = * & c2 = *
| c1 = 1 ⟹ c3 = 1
| c1 = 2 ⟹ c3 = 2
| c2 = 1 ⟹ c3 = 1
| c2 = 2 ⟹ c3 = 2
| c3 = 1 ⟹ T
| c3 = 2 ⟹ T
⟹ c0 = 1 & 1 <= c1 <=2 & 1 <= c2 <= 2 & 1 <= c3 <= 2
^ This is WRONG as we lose important information in
  the process. Here c1=2&c3=1 would be valid, but in
  reality it is not.
... Though does it matter? As long as we properly follow
    the constraints in phase 2, unifying the known constraints
    from this with whatever we picked, we end up fine. After
    all, we have proven that a solution exists here, we now
    "just" need to pick it, eagerly unifying constraints along
    the way.
... Whatever  the case, it does matter during the actual
    search, so we can't primitively unify during unwinding.

# Single solution problem that requires backtracking
| c0 = 1 ⟹ c1 = * & c2 = *
| c1 = 1 ⟹ c3 = 1
| c1 = 2 ⟹ c3 = 2
| c2 = 1 ⟹ c3 = 2
| c2 = 2 ⟹ c3 = 2
| c3 = 1 ⟹ T
| c3 = 2 ⟹ T
⟹ c0 = 1 & c1 = 2 & c2 = 1 & c3 = 2

# Unsatisfiable problem
| c0 = 1 ⟹ c1 = * & c2 = *
| c1 = 1 ⟹ c3 = 1
| c1 = 2 ⟹ c3 = 1
| c2 = 1 ⟹ c3 = 2
| c2 = 2 ⟹ c3 = 2
| c3 = 1 ⟹ T
| c3 = 2 ⟹ T
⟹ NIL

Traverse depth-first, marking nodes along the way tentative to
detect cycles. Then, on unwind start unifying constraints. if
we have multiple applicable dependencies, unify into a single
set of OR-ed branches. Should that set ever become empty, we
have an unsatisfiable branch. If the empty set arrives at
the top node, the whole solution cannot be solved.

|#

