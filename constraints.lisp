#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(define-condition constraints-incompatible (error)
  ((a :initarg :a :reader a)
   (b :initarg :b :reader b)))

(defgeneric %parse-constraint (comp args))

(defun parse-constraint (constraint)
  (if (listp constraint)
      (destructuring-bind (constraint-type . args) constraint
        (%parse-constraint constraint-type args))
      (%parse-constraint constraint ())))

(defmacro define-constraint-parser (constraint args &body body)
  `(defmethod %parse-constraint ((_ (eql ,constraint)) args)
     (destructuring-bind ,args args
       ,@body)))

(defclass version-constraint ()
  ())

(defgeneric version-match-p (version constraint))
(defgeneric constraint-subset-p (sub sup))
(defgeneric unify (a b))
(defgeneric widen (a b))

(defmethod print-object ((constraint version-constraint) stream)
  (print-unreadable-object (constraint stream)
    (format stream "CONSTRAINT ~a" (to-string constraint))))

(defun unify* (&rest constraints)
  (if constraints
      (let ((constraint (first constraints)))
        (dolist (other (rest constraints) constraint)
          (setf constraint (unify constraint other))))
      (make-instance 'version-unspecific-constraint)))

(defun match-constraint (versions constraint &key key)
  (let ((key (etypecase key
               (null #'identity)
               (symbol (fdefinition key))
               (function key))))
    (loop for version in versions
          when (version-match-p (funcall key version) constraint)
          collect version)))

(defclass version-unspecific-constraint ()
  ())

(defmethod version-match-p ((version version) (constraint version-unspecific-constraint))
  T)

(defmethod constraint-subset-p ((sub version-unspecific-constraint) (sub version-unspecific-constraint))
  T)

(defmethod constraint-subset-p ((sub version-constraint) (sub version-unspecific-constraint))
  T)

(defmethod constraint-subset-p ((sub version-unspecific-constraint) (sub version-constraint))
  NIL)

(defmethod unify ((a version-unspecific-constraint) (b version-constraint))
  b)

(defmethod unify ((b version-constraint) (a version-unspecific-constraint))
  b)

(defmethod widen ((a version-unspecific-constraint) (b version-constraint))
  a)

(defmethod widen ((b version-constraint) (a version-unspecific-constraint))
  a)

(defmethod to-string ((constraint version-unspecific-constraint))
  "T")

(define-constraint-parser T ()
  (make-instance 'version-unspecific-constraint))

(defclass version-equal-constraint ()
  ((version :initarg :version :initform *unknown-version* :reader version)))

(defmethod version-match-p ((version version) (constraint version-equal-constraint))
  (version= version (version constraint)))

(defmethod constraint-subset-p ((sub version-equal-constraint) (sup version-equal-constraint))
  (version= (version sub) (version sup)))

(defmethod unify ((a version-equal-constraint) (b version-equal-constraint))
  (unless (version= (version a) (version b))
    (error 'constraints-incompatible :a a :b b))
  a)

(defmethod widen ((a version-equal-constraint) (b version-equal-constraint))
  (when (version= (version a) (version b))
    a))

(defmethod to-string ((constraint version-equal-constraint))
  (format NIL "=~a" (to-string (version constraint))))

(define-constraint-parser = (version)
  (make-instance 'version-equal-constraint :version (parse-version version)))

(defclass version-range-constraint ()
  ((min-version :initarg :min-version :initform *minimal-version* :reader min-version)
   (max-version :initarg :max-version :initform *maximal-version* :reader max-version)))

(defmethod initialize-instance :after ((constraint version-range-constraint) &key)
  (when (version<= (max-version constraint) (min-version constraint))
    (error "Versions do not form a valid range: [~a, ~a]" (min-version constraint) (max-version constraint))))

(defmethod version-match-p ((version version) (constraint version-range-constraint))
  (and (version<= (min-version constraint) version)
       (version<= version (max-version constraint))))

(defmethod constraint-subset-p ((sub version-range-constraint) (sup version-range-constraint))
  (and (version<= (min-version sup) (min-version sub))
       (version<= (max-version sub) (max-version sup))))

(defmethod constraint-subset-p ((sub version-equal-constraint) (sup version-range-constraint))
  (and (version<= (min-version sup) (version sub))
       (version<= (version sub) (max-version sup))))

(defmethod constraint-subset-p ((sub version-range-constraint) (sup version-equal-constraint))
  NIL)

(defmethod unify ((a version-range-constraint) (b version-range-constraint))
  (let ((min (version-max (min-version a) (min-version b)))
        (max (version-min (max-version a) (max-version b))))
    (cond ((version= min max)
           (make-instance 'version-equal-constraint :version min))
          ((version< min max)
           (make-instance 'version-range-constraint :min-version min :max-version max))
          (T
           (error 'constraints-incompatible :a a :b b)))))

(defmethod unify ((a version-range-constraint) (b version-equal-constraint))
  (unless (and (version<= (min-version a) (version b))
               (version<= (version b) (max-version a)))
    (error 'constraints-incompatible :a a :b b))
  b)

(defmethod unify ((b version-equal-constraint) (a version-range-constraint))
  (unify a b))

(defmethod widen ((a version-range-constraint) (b version-range-constraint))
  (cond ((constraint-subset-p a b) b)
        ((constraint-subset-p b a) a)
        ((or (and (version<= (min-version a) (max-version b))
                   (version<= (max-version b) (max-version a)))
              (and (version<= (min-version a) (min-version b))
                   (version<= (min-version b) (max-version a))))
         (make-instance 'version-range-constraint
                        :min-version (version-min (min-version b) (min-version a))
                        :max-version (version-max (max-version b) (max-version a))))))

(defmethod widen ((a version-range-constraint) (b version-equal-constraint))
  (when (constraint-subset-p b a)
    a))

(defmethod widen ((b version-equal-constraint) (a version-range-constraint))
  (widen a b))

(defmethod to-string ((constraint version-equal-constraint))
  (format NIL "[~a,~a]" (to-string (min-version constraint)) (to-string (max-version constraint))))

(define-constraint-parser <= (version)
  (make-instance 'version-range-constraint :max-version (parse-version version)))
(define-constraint-parser >= (version)
  (make-instance 'version-range-constraint :min-version (parse-version version)))
(define-constraint-parser [ (min max)
  (make-instance 'version-range-constraint :min-version (parse-version min)
                                           :max-version (parse-version max)))

(defclass constraint-union ()
  ((set :initarg :set :reader set)))

(defmethod initialize-instance :after ((union constraint-union) &key)
  (let ((set ()))
    (flet ((add (new)
             (etypecase new
               (version-unspecific-constraint
                (setf set (list new)))
               (version-equal-constraint
                (loop for other in set
                      do (when (constraint-subset-p new other)
                           (return))
                      finally (push new set)))
               (version-range-constraint
                (loop for cons on set
                      for wider = (widen (car cons) new)
                      do (when wider
                           (return (setf (car cons) wider)))
                      finally (push new set))))))
      (dolist (constraint (set union))
        (etypecase constraint
          (constraint-union
           (dolist (sub constraint)
             (add sub)))
          (version-constraint
           (add constraint)))))
    (cond ((rest set)
           (setf (slot-value union 'set) set))
          (set
           (etypecase (first set)
             (version-unspecific-constraint
              (change-class union 'version-unspecific-constraint))
             (version-equal-constraint
              (change-class union 'version-equal-constraint
                            :version (version (first set))))
             (version-range-constraint
              (change-class union 'version-range-constraint
                            :min-version (min-version (first set))
                            :max-version (max-version (first set))))))
          (T
           (error "Can't construct a constraint union: set is empty.")))))

(defmethod version-match-p ((version version) (constraint constraint-union))
  (loop for constraint in (set constraint)
        thereis (version-match-p version constraint)))

(defmethod constraint-subset-p ((a constraint-union) (b constraint-union))
  (loop for constraint in (set a)
        always (constraint-subset-p a b)))

(defmethod constraint-subset-p ((a version-constraint) (b constraint-union))
  (loop for constraint in (set b)
        thereis (constraint-subset-p a constraint)))

(defmethod constraint-subset-p ((a constraint-union) (b version-equal-constraint))
  NIL)

(defmethod constraint-subset-p ((a constraint-union) (b version-range-constraint))
  (loop for constraint in (set a)
        always (constraint-subset-p constraint b)))

(defmethod unify ((a constraint-union) (b constraint-union))
  (let ((set ()))
    (dolist (ac (set a))
      (dolist (bc (set b))
        (handler-case (push (unify ac bc) set)
          (constraints-incompatible ()))))
    (cond ((rest set)
           (make-instance 'constraint-union :set set))
          (set
           (first set))
          (T
           (error 'constraints-incompatible :a a :b b)))))

(defmethod unify ((a constraint-union) (b version-equal-constraint))
  (loop for constraint in (set a)
        do (when (constraint-subset-p b constraint)
             (return b))
        finally (error 'constraints-incompatible :a a :b b)))

(defmethod unify ((a constraint-union) (b version-range-constraint))
  (loop for constraint in (set a)
        do (cond ((constraint-subset-p b constraint)
                  (return b))
                 ((constraint-subset-p constraint b)
                  (return constraint)))
        finally (error 'constraints-incompatible :a a :b b)))

(defmethod unify ((b version-range-constraint) (a constraint-union))
  (unify a b))

(defmethod unify ((a constraint-union) (b version-equal-constraint))
  (make-instance 'constraint-union :set (list* b (set a))))

(defmethod unify ((b version-equal-constraint) (a constraint-union))
  (unify a b))

(defmethod widen ((a constraint-union) (b constraint-union))
  (make-instance 'constraint-union :set (append (set a) (set b))))

(defmethod widen ((a constraint-union) (b version-constraint))
  (make-instance 'constraint-union :set (list* b (set a))))

(defmethod to-string ((constraint version-equal-constraint))
  (with-output-to-string (stream)
    (format stream "{")
    (loop for (constraint rest) on (set constraint)
          do (write-string (to-string constraint) stream)
             (when rest (write-char #\, stream)))
    (format stream "}")))

(define-constraint-parser or (&rest constraints)
  (make-instance 'constraint-union :set (mapcar #'parse-constraint constraints)))
