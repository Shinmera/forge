;;;; Description:
;;; This file defines common mixin classes and behaviours that are generally
;;; useful outside of a specific language module. Most notably it defines the
;;; standard plan computation algorithm with the BASIC-POLICY.

(in-package #:org.shirakumo.forge)

(defclass basic-database (database)
  ((effects :initform (make-hash-table :test 'eq) :reader effects)
   (version-cache :initform (make-hash-table :test 'eq) :reader version-cache)))

;; Storage as follows:
;; Effects: TYPE -> PARAMETERS -> [ EFFECT ]

(defmethod map-effects (function (database basic-database) &optional type parameters version)
  (let ((function (etypecase function
                    (function function)
                    (symbol (fdefinition function)))))
    (cond (version
           (let ((tables (gethash type (effects database))))
             (when tables
               (let ((effects (gethash (normalize-parameters type parameters) tables)))
                 (loop for effect in effects
                       do (when (version-match-p (version effect) version)
                            (funcall function effect)))))))
          (parameters
           (let ((tables (gethash type (effects database))))
             (when tables
               (let ((effects (gethash (normalize-parameters type parameters) tables)))
                 (loop for effect in effects
                       do (funcall function effect))))))
          (type
           (let ((tables (gethash type (effects database))))
             (when tables
               (loop for effects being the hash-values of tables
                     do (loop for effect in effects
                              do (funcall function effect))))))
          (T
           (loop for tables being the hash-values of (effects database)
                 do (loop for effects being the hash-values of tables
                          do (loop for effect in effects
                                   do (funcall function effect))))))))

(defmethod register-effect ((database basic-database) (effect effect))
  (let* ((tables (or (gethash (type-of effect) (effects database))
                     (setf (gethash (type-of effect) (effects database))
                           (make-hash-table :test 'equal))))
         (effects (gethash (parameters effect) tables))
         (existing (find effect effects :test #'version=)))
    (cond ((eq existing effect))
          ((null existing)
           (push effect (gethash (parameters effect) tables)))
          (T
           (error "Different effect with same parameters and version already exists!")))))

(defmethod achieved-version ((database basic-database) (effect effect))
  (gethash effect (version-cache effect) *unknown-version*))

(defmethod (setf achieved-version) ((version version) (database basic-database) (effect effect))
  (setf (gethash effect (version-cache effect)) version))

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
  ((operation-cache :initform (make-hash-table :test 'eq) :accessor operation-cache)))

(defmethod select-source ((policy basic-policy) (effect effect) sources)
  (first sources))

(defmethod make-operation ((operation symbol) (policy basic-policy))
  (or (gethash operation (operation-cache policy))
      (setf (gethash operation (operation-cache policy))
            (make-operation (make-instance operation) policy))))

(defmethod select-effect-set ((policy basic-policy) sets)
  ;; FIXME: This is not stable as the order of effects within a set is not stable.
  (flet ((set> (a b)
           (loop for ae in a
                 for be in b
                 do (cond ((eq ae be))
                          ((version= (version ae) (version be)))
                          ((version< (version ae) (version be))
                           (return NIL))
                          (T
                           (return T))))))
    (let ((min (first sets)))
      (dolist (set (rest sets) min)
        (when (set> set min)
          (setf min set))))))

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
  (with-retry ("Retry computing the plan")
    (let ((visit (make-hash-table :test 'equal)))
      ;; Note: We first compute the set of possible version constraints for all involved effects. If this set turns
      ;;       out to be empty for the root effect, we know it's unsatisfiable overall. Most likely though we are
      ;;       going to get a huge set of possible alternatives to pick from. We actually resolve this set in a second
      ;;       step where we compute the actual plan.
      (labels ((visit (effect)
                 (etypecase (gethash effect visit :none)
                   ((eql :tentative)
                    (error 'dependency-cycle-detected :effect effect))
                   (list
                    (gethash effect visit :none))
                   ((eql :none)
                    (setf (gethash effect visit) :tentative)
                    ;; Note: We eagerly select the source here, which can lead to an unsatisfiable plan even when
                    ;;       another source might provide a satisfiable plan. However, for a first attempt, and for
                    ;;       my own sanity's sake, I'm going to ignore this for the time being.
                    (let* ((source (select-source policy effect (sources effect)))
                           (component (second source))
                           (operation (make-operation (first source) policy))
                           ;; Note: We only consider hard dependencies here. Optionals are pulled in in another phase
                           ;;       once we have already settled on a version set for everything else. This means we
                           ;;       potentially don't select optionals that might be possible in another set, but it
                           ;;       massively simplifies the plan computation, and at this point I'm too tired of trying
                           ;;       to come up with a holistic solution to continue doing the "absolutely right thing".
                           (dependencies (remove-if-not #'hard-p (dependencies operation component)))
                           (depchoices T))
                      (dolist (dependency dependencies)
                        (let ((choices ()))
                          (with-retry ("Retry resolving the dependency.")
                            (do-effects (effect *database* (effect-type dependency) (parameters dependency) (version dependency))
                              (dolist (choice (visit effect))
                                (push choice choices)))
                            (unless choices
                              (restart-case
                                  (warn 'unsatisfiable-dependency :dependency dependency :operation operation :component component)
                                (use-value (effect)
                                  :report "Specify an effect to use."
                                  (check-type effect effect)
                                  (dolist (choice (visit effect))
                                    (push choice choices))))))
                          ;; This is where the combinatorial explosion happens
                          (setf depchoices (unify-dependency-sets depchoices choices))))
                      (setf (gethash effect visit)
                            (if (eql T depchoices) (list (list effect))
                                (loop for choice in depchoices
                                      collect (list* effect choice)))))))))
        (unless (visit effect)
          (error 'unsatisfiable-effect :effect effect)))
      ;; Note: Next we select one set of the viable effects set and traverse the graph again. We now select the effect
      ;;       that matches our dependency directly from the set that we used and don't bother with checking cycles.
      ;;       We also now check optional dependencies and see if all of their dependencies are already contained in
      ;;       our set. If so, we pull their steps in. Once we have constructed steps for each effect, we can then
      ;;       figure out the head and tail steps and construct the actual plan object.
      (let ((effects (select-effect-set policy (gethash effect visit)))
            (step-table (make-hash-table :test 'eq)))
        (labels ((find-effect (dependency)
                   ;; FIXME: This seems slow as balls? Surely there's a better way...
                   (let ((type (effect-type dependency))
                         (parameters (parameters dependency)))
                     (dolist (effect effects (error "WTF?"))
                       (when (and (eql (type-of effect) type)
                                  (equal (parameters effect) parameters))
                         (return effect)))))
                 (maybe-visit (dependency)
                   (do-effects (effect *database* (effect-type dependency) (parameters dependency) (version dependency))
                     (let* ((source (select-source policy effect (sources effect)))
                            (component (second source))
                            (operation (make-operation (first source) policy))
                            (dependencies (dependencies operation component)))
                       (when (loop for dependency in dependencies
                                   always (find-effect dependency))
                         (return (visit effect))))))
                 (visit (effect)
                   (or (gethash effect step-table)
                       (let* ((source (select-source policy effect (sources effect)))
                              (component (second source))
                              (operation (make-operation (first source) policy))
                              (step (make-step operation component effect))
                              (dependencies (dependencies operation component)))
                         (dolist (dependency dependencies)
                           (let ((predecessor (if (hard-p dependency)
                                                  (visit (find-effect dependency))
                                                  (maybe-visit dependency))))
                             (when predecessor
                               (connect predecessor step))))
                         (setf (gethash effect step-table) step)))))
          (visit effect))
        ;; Note: Now we do the additional step of "expanding" compound steps to allow broad-phasing of plans. We
        ;;       then replace the original broad steps by tying up the successors and predecessors.
        (loop for key being the hash-keys of step-table
              for step being the hash-values of step-table
              do (when (typep step 'compound-step)
                   ;; Unhook the step from the plan
                   (remhash key step-table)
                   (dolist (predecessor (predecessors step))
                     (setf (successors predecessor) (delete step (successors predecessor))))
                   (dolist (successor (successors step))
                     (setf (predecessors successor) (delete step (predecessors successor))))
                   ;; Compute the inner plan and tie the steps together
                   (let ((plan (compute-plan (inner-effect step) policy)))
                     (loop for first across (first-steps plan)
                           do (setf (gethash first step-table) first)
                              (dolist (predecessor (predecessors step))
                                (connect predecessor first)))
                     (loop for final across (final-steps plan)
                           do (setf (gethash final step-table) final)
                              (dolist (successor (successors step))
                                (connect final successor))))))
        ;; Note: Finally we figure out the heads and tails of the whole plan by searching through all known steps
        ;;       for ones without any successors or predecessors. This concludes the full plan.
        (let ((first-steps (make-array 0 :adjustable T :fill-pointer T))
              (final-steps (make-array 0 :adjustable T :fill-pointer T)))
          (loop for step being the hash-values of step-table
                do (cond ((null (predecessors step))
                          (vector-push-extend step first-steps))
                         ((null (successors step))
                          (vector-push-extend step final-steps))))
          (make-instance 'plan
                         :first-steps first-steps
                         :final-steps final-steps))))))

(defclass dummy-executor (executor)
  ())

(defmethod execute ((plan plan) (executor dummy-executor))
  plan)

(defclass linear-executor (executor)
  ((force :initarg :force :initform NIL :accessor force)
   (client :initarg :client :initform (or (and *server* (first (list-clients *server*)))
                                          (support:arg! :client))
           :accessor client)))

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

(defmethod execute ((plan plan) (executor linear-executor))
  (promise:do-promised (step (compute-step-sequence plan))
    (handler-bind ((error #'invoke-debugger))
      (execute step executor))))

(defclass file-component (component)
  ((path :initarg :path :accessor path)))

(define-print-object-method* file-component
  "~s ~a ~a" name path (to-string (version file-component)))

(defmethod full-path ((component file-component))
  (path component))

(defmethod version ((component file-component))
  (let ((version (call-next-method))
        (hash (make-instance 'hashed-version :value (hash-file (path component)))))
    (if (typep version 'unknown-version)
        hash
        (make-instance 'compound-version :versions (list version hash)))))

(defmethod hash ((component file-component))
  (hash-file (path component)))

(defclass parent-component (component)
  ((children :initform (make-hash-table :test 'equal) :accessor children)))

(defclass child-component (component)
  ((parent :initarg :parent :initform (support:arg! :parent) :reader parent)))

(defmethod full-path ((component child-component))
  (merge-pathnames (call-next-method)
                   (path (parent component))))

(defclass dependencies-component (component)
  ((depends-on :initform () :accessor depends-on)))

(defmethod shared-initialize :after ((component dependencies-component) slots &key (depends-on NIL dependencies-p) prior)
  (cond (dependencies-p
         (setf (depends-on component)
               (loop for dependency in depends-on
                     collect (normalize-dependency-spec component dependency))))
        (prior
         (setf (depends-on component)
               (list (normalize-dependency-spec component prior))))))

(defgeneric normalize-dependency-spec (component dependency))
