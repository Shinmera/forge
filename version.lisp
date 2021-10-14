#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defclass version ()
  ())

(define-print-object-method* version
  "~a" to-string)

(defgeneric version= (a b))
(defgeneric version< (a b))
(defgeneric to-string (version))
(defgeneric parse-version (thing))

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

(defmethod support:generic< ((a version) (b version))
  (version<= a b))

(defmethod parse-version ((version version))
  version)

(defclass unknown-version (version)
  ())

(defmethod version= ((a unknown-version) b) NIL)
(defmethod version= (b (a unknown-version)) NIL)
(defmethod version< ((a unknown-version) b) T)
(defmethod version< (b (a unknown-version)) NIL)
(defmethod to-string ((v unknown-version)) "?")

(defvar *unknown-version* (make-instance 'unknown-version))

(defmethod parse-version ((version (eql NIL)))
  *unknown-version*)

(defclass maximal-version (version)
  ())

(defmethod version= ((a maximal-version) (b maximal-version)) T)
(defmethod version= ((a maximal-version) (b version)) NIL)
(defmethod version= ((b version) (a maximal-version)) NIL)
(defmethod version< ((b version) (a maximal-version)) T)
(defmethod version< ((a maximal-version) (b version)) NIL)
(defmethod to-string ((v maximal-version)) "+∞")

(defmethod parse-version ((version (eql :max)))
  *maximal-version*)

(defvar *maximal-version* (make-instance 'maximal-version))

(defclass minimal-version (version)
  ())

(defmethod version= ((a minimal-version) (b minimal-version)) T)
(defmethod version= ((a minimal-version) (b version)) NIL)
(defmethod version= ((b version) (a minimal-version)) NIL)
(defmethod version< ((b version) (a minimal-version)) NIL)
(defmethod version< ((a minimal-version) (b version)) T)
(defmethod to-string ((v minimal-version)) "-∞")

(defmethod parse-version ((version (eql :min)))
  *minimal-version*)

(defvar *minimal-version* (make-instance 'minimal-version))

(defclass integer-version (version)
  ((value :initarg :value :initform 0 :accessor value)))

(defmethod version= ((a integer-version) (b integer-version))
  (= (value a) (value b)))

(defmethod version< ((a integer-version) (b integer-version))
  (< (value a) (value b)))

(defmethod to-string ((v integer-version))
  (princ-to-string (value v)))

(defmethod parse-version ((version integer))
  (make-instance 'integer-version :value version))

(defclass hashed-version (version)
  ((value :initarg :value :initform 0 :accessor value)))

(defmethod version= ((a hashed-version) (b hashed-version))
  (string= (value a) (value b)))

(defmethod version< ((a hashed-version) (b hashed-version))
  NIL)

(defmethod to-string ((v hashed-version))
  (value v))

(defmethod parse-version ((version string))
  (make-instance 'hashed-version :value version))

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

(defmethod parse-version ((version cons))
  (make-instance 'separated-version :value version))

(defclass compound-version (version)
  ((versions :initarg :versions :initform (support:arg! :versions) :reader versions)))

(defmethod version= ((a compound-version) (b compound-version))
  (loop for (ai ar) on (versions a)
        for (bi br) on (versions b)
        always (and (version= ai bi)
                    (or (and ar br)
                        (and (null ar) (null br))))))

(defmethod version< ((a compound-version) (b compound-version))
  (loop for ai in (versions a)
        for bi in (versions b)
        do (cond ((version< ai bi) (return T))
                 ((version< bi ai) (return NIL)))))

(defmethod to-string ((v compound-version))
  (format NIL "~{~a~^-~}" (mapcar #'to-string (versions v))))

(defun version-from-string (version)
  (flet ((alpha-p (char)
           (or (<= (char-code #\a) (char-code char) (char-code #\z))
               (<= (char-code #\A) (char-code char) (char-code #\Z)))))
    (cond ((find #\- version)
           (make-instance 'compound-version :versions (mapcar #'version-from-string (cl-ppcre:split "[-]+" version))))
          ((find #\. version)
           (let* ((hashes (loop for part in (cl-ppcre:split "[.]+" version)
                                collect (value (version-from-string part))))
                  (integers (loop for part = (car hashes)
                                  while (integerp part)
                                  collect (pop hashes)))
                  (version (make-instance 'separated-version :value integers)))
             (if hashes
                 (make-instance 'compound-version :versions (list* version (mapcar #'parse-version hashes)))
                 version)))
          ((every #'digit-char-p version)
           (make-instance 'integer-version :value (parse-integer version)))
          ((and (= 1 (length version)) (alpha-p (char version 0)))
           (make-instance 'integer-version :value (- (char-code (char-downcase (char version 0))) (char-code #\a))))
          (T
           (make-instance 'hashed-version :value version)))))

(defclass versioned-object ()
  ((version :initform *unknown-version* :accessor version)))

(defmethod initialize-instance ((object versioned-object) &key version)
  (call-next-method)
  (when version
    (setf (version object)
          (typecase version
            (version version)
            (null *unknown-version*)
            (T (parse-version version))))))
