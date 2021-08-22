#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defclass version ()
  ())

(defgeneric version= (a b))
(defgeneric version< (a b))
(defgeneric to-string (version))

(defgeneric version<= (a b)
  (:method ((a version) (b version))
    (or (version= a b)
        (version< a b))))

(defgeneric version-min (a b)
  (:method ((a version) (b version))
    (if (version< a b) a b)))

(defgeneric version-max (a b)
  (:method ((a version) (b version))
    (if (version< a b) b a)))

(defclass unknown-version (version)
  ())

(defmethod version= ((a unknown-version) b) NIL)
(defmethod version= (b (a unknown-version)) NIL)
(defmethod version< ((a unknown-version) b) T)
(defmethod version< (b (a unknown-version)) NIL)
(defmethod to-string ((v unknown-version)) "")

(defvar *unknown-version* (make-instance 'unknown-version))

(defclass maximal-version (version)
  ())

(defmethod version= ((a maximal-version) (b maximal-version)) T)
(defmethod version= ((a maximal-version) (b version)) NIL)
(defmethod version= ((b version) (a maximal-version)) NIL)
(defmethod version< ((b version) (a maximal-version)) T)
(defmethod version< ((a maximal-version) (b version)) NIL)
(defmethod to-string ((v unknown-version)) "+∞")

(defvar *maximal-version* (make-instance 'maximal-version))

(defclass minimal-version (version)
  ())

(defmethod version= ((a minimal-version) (b minimal-version)) T)
(defmethod version= ((a minimal-version) (b version)) NIL)
(defmethod version= ((b version) (a minimal-version)) NIL)
(defmethod version< ((b version) (a minimal-version)) NIL)
(defmethod version< ((a minimal-version) (b version)) T)
(defmethod to-string ((v unknown-version)) "-∞")

(defvar *minimal-version* (make-instance 'minimal-version))

(defclass integer-version (version)
  ((value :initarg :value :initform 0 :accessor value)))

(defmethod version= ((a integer-version) (b integer-version))
  (= (value a) (value b)))

(defmethod version< ((a integer-version) (b integer-version))
  (< (value a) (value b)))

(defmethod to-string ((v integer-version))
  (princ-to-string (value v)))

(defclass hashed-version (version)
  ((value :initarg :value :initform 0 :accessor value)))

(defmethod version= ((a hashed-version) (b hashed-version))
  (string= (value a) (value b)))

(defmethod version< ((a hashed-version) (b hashed-version))
  NIL)

(defmethod to-string ((v hashed-version))
  (value v))

(defclass separated-version (version)
  ((value :initarg :value :initform '(1) :accessor value)))

(defmethod version= ((a separated-version) (b separated-version))
  (loop for (ai ar) on (value a)
        for (bi br) on (value b)
        always (and (= ai bi)
                    (or (and ar br)
                        (and (null ar) (null br))))))

(defmethod version< ((a separated-version) (b separated-version))
  (loop for ai in (value a)
        for bi in (value b)
        do (cond ((< ai bi) (return T))
                 ((< bi ai) (return NIL)))))

(defmethod to-string ((v separated-version))
  (format NIL "~{~d~^.~}" (value v)))

(defclass versioned-object ()
  ((version :initarg :version :initform *unknown-version* :accessor version)))

