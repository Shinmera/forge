#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.protocol)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *encoding-type-id-map* (make-hash-table :test 'eq))
  (defvar *encoding-type-id-counter* 0)
  
  (defun ensure-encoding-type-id (type)
    (or (gethash type *encoding-type-id-map*)
        (setf (gethash type *encoding-type-id-map*)
              (incf *encoding-type-id-counter*))))

  (defun encoding-type-id (type)
    (or (gethash type *encoding-type-id-map*)
        (error "The type ~s is not assigned an ID." type)))

  (defun (setf encoding-type-id) (id type)
    (setf (gethash type *encoding-type-id-map*) id)))

(defmacro define-binary-accessor (size)
  `(progn
     (defun ,(intern (format NIL "WU~d" size)) (int stream)
       ,@(loop for i from 0 below size by 8
               collect `(write-byte (ldb (byte 8 ,i) int) stream)))
     (defun ,(intern (format NIL "RU~d" size)) (stream)
       (+ ,@(loop for i from 0 below size by 8
                  collect `(ash (read-byte stream) ,i))))
     (defun ,(intern (format NIL "RI~d" size)) (stream)
       (let ((bits (+ ,@(loop for i from 0 below size by 8
                              collect `(ash (read-byte stream) ,i)))))
         (dpb bits (byte ,size 0)
              (if (logbitp ,(1- size) bits) -1 0))))))

;; FIXME: circularity?

(define-binary-accessor 8)
(define-binary-accessor 16)
(define-binary-accessor 32)
(define-binary-accessor 64)

(defmethod encode-message (value (pathname pathname))
  (with-open-file (stream pathname :direction :output :element-type '(unsigned-byte 8)
                                   :if-exists :supersede)
    (encode-message value stream)))

(defmethod decode-message ((id (eql T)) (pathname pathname))
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (decode-message (ru16 stream) stream)))

(defmethod decode-message (type source)
  (if (typep type 'integer)
      (no-applicable-method #'decode-message type source)
      (decode-message (encoding-type-id type) source)))

(defmacro define-encoding (class (value stream) &body en/de)
  (destructuring-bind (encoder decoder) en/de
    (let ((id (ensure-encoding-type-id class)))
      `(flet ((encode (,value ,stream)
                (flet ((encode (,value)
                         (encode-message ,value ,stream))
                       (encode* (,value)
                         (encode-payload ,value ,stream)))
                  (declare (ignorable #'encode #'encode*))
                  ,encoder)))
         (setf (encoding-type-id ',class) ,id)
         (defmethod encode-message ((,value ,class) (,stream stream))
           (wu16 ,id ,stream)
           (encode ,value ,stream))
         (defmethod encode-payload ((,value ,class) (,stream stream))
           (encode ,value ,stream))
         (defmethod decode-message ((id (eql ,id)) (,stream stream))
           (flet ((decode (&optional (,value (ru16 ,stream)))
                    (decode-message ,value ,stream)))
             (declare (ignorable #'decode))
             ,decoder))))))

(define-encoding null (value stream)
  ()
  NIL)

(define-encoding cons (value stream)
  (progn (encode (car value))
         (encode (cdr value)))
  (cons (decode) (decode)))

(define-encoding symbol (value stream)
  (progn (encode* (if (symbol-package value)
                      (package-name (symbol-package value))
                      ""))
         (encode* (symbol-name value)))
  (let ((package (decode (encoding-type-id 'string)))
        (name (decode (encoding-type-id 'string))))
    (if (string= package "")
        (make-symbol name)
        (intern name (find-package package)))))

;; FIXME: Bignum support
(define-encoding integer (value stream)
  (progn
    (check-type value (signed-byte 64))
    (wu64 value stream))
  (ri64 stream))

(define-encoding ratio (value stream)
  (progn (encode* (numerator value))
         (encode* (denominator value)))
  (/ (decode (encoding-type-id 'integer))
     (decode (encoding-type-id 'integer))))

(define-encoding complex (value stream)
  (progn (encode (realpart value))
         (encode (imagpart value)))
  (complex (decode)
           (decode)))

;; KLUDGE: Base CL gives us only very inefficient means of doing this.
(define-encoding single-float (value stream)
  (multiple-value-bind (m e s) (integer-decode-float value)
    (wu32 m stream)
    (wu8 e stream)
    (wu8 s stream))
  (let ((m (ru32 stream))
        (e (ri8 stream))
        (s (ri8 stream)))
    (float (* m s (expt 2 (abs e))) 0f0)))

(define-encoding double-float (value stream)
  (multiple-value-bind (m e s) (integer-decode-float value)
    (wu64 m stream)
    (wu16 e stream)
    (wu8 s stream))
  (let ((m (ru64 stream))
        (e (ri16 stream))
        (s (ri8 stream)))
    (float (* m s (expt 2 (abs e))) 0f0)))

(define-encoding character (value stream)
  (wu32 (char-code value) stream)
  (code-char (ru32 stream)))

(define-encoding string (value stream)
  (progn (wu32 (length value) stream)
         ;; FIXME: convert to utf-8
         (loop for char across value
               do (wu32 (char-code char) stream)))
  (let ((arr (make-array (ru32 stream) :element-type 'character)))
    (dotimes (i (length arr) arr)
      (setf (aref arr i) (code-char (ru32 stream))))))

;; FIXME: compressed bit vector support
;; FIXME: fill-pointer support
(let ((ub8-id (ensure-encoding-type-id 'ub8-vector))
      (gen-id (ensure-encoding-type-id 'vector)))
  (defmethod encode-message ((value vector) (stream stream))
    (typecase value
      ((vector (unsigned-byte 8))
       (wu16 ub8-id stream)
       (wu32 (length value) stream)
       (loop for element across value
             do (wu8 element stream)))
      (T
       (wu16 gen-id stream)
       (wu32 (length value) stream)
       (loop for object across value
             do (encode-message object stream)))))
  (defmethod decode-message ((id (eql ub8-id)) (stream stream))
    (let ((arr (make-array (ru32 stream) :element-type '(unsigned-byte 8))))
      (read-sequence arr stream)
      arr))
  (defmethod decode-message ((id (eql gen-id)) (stream stream))
    (let ((arr (make-array (ru32 stream))))
      (dotimes (i (length arr) arr)
        (setf (aref arr i) (decode-message (ru16 stream) stream))))))

(define-encoding array (value stream)
  (let ((dimensions (array-dimensions value)))
    (wu8 (length dimensions) stream)
    (dolist (dimension dimensions)
      (wu32 dimension stream))
    (dotimes (i (array-total-size value))
      (encode (row-major-aref value i))))
  (let ((dimensions ()))
    (dotimes (i (ru8 stream))
      (push (ru32 stream) dimensions))
    (let ((array (make-array (nreverse dimensions))))
      (dotimes (i (array-total-size array) array)
        (setf (row-major-aref array i) (decode))))))

(define-encoding hash-table (value stream)
  (progn (wu32 (hash-table-count value) stream)
         (wu8 (ecase (hash-table-test value)
                (eq 0)
                (eql 1)
                (equal 2)
                (equalp 3))
              stream)
         (loop for k being the hash-keys of value
               for v being the hash-values of value
               do (encode k)
                  (encode v)))
  (let* ((size (ru32 stream))
         (table (make-hash-table :size size
                                 :test (ecase (ru8 stream)
                                         (0 'eq)
                                         (1 'eql)
                                         (2 'equal)
                                         (3 'equalp)))))
    (loop repeat size
          for k = (decode)
          for v = (decode)
          do (setf (gethash k table) v))
    table))

(labels ((unspecific-p (component)
           (or (eq component NIL)
               (eq component :unspecific)
               (and (stringp component)
                    (= 0 (length component)))))
         (maybe-component (component)
           (if (unspecific-p component) NIL component))
         (normalize-directory-spec (dir)
           (etypecase dir
             (string `(:absolute ,dir))
             ((member :wild :wild-inferiors) `(:relative ,dir))
             (cons
              (if (member (first dir) '(:absolute :relative))
                  dir
                  #+gcl `(:relative ,dir)
                  #-gcl (error "Invalid directory component ~s" dir)))
             (T (unless (unspecific-p dir)
                  dir)))))
  (define-encoding pathname (value stream)
    ;; We ignore the host as it cannot typically be encoded portably.
    (progn (encode (maybe-component (pathname-device value)))
           (encode (maybe-component (pathname-name value)))
           (encode (maybe-component (pathname-type value)))
           (encode (maybe-component (pathname-version value)))
           (encode (normalize-directory-spec (pathname-directory value))))
    (make-pathname :device (decode)
                   :name (decode)
                   :type (decode)
                   :version (decode)
                   :directory (decode))))

(define-encoding package (value stream)
  (encode* (package-name value))
  (let ((name (decode (encoding-type-id 'string))))
    (or (find-package name)
        (error "Package was transferred that does not exist: ~%  ~s" name))))

;; KLUDGE: round-trip through print/read since we can't read these values out normally.
(define-encoding random-state (value stream)
  (encode* (with-standard-io-syntax (prin1-to-string value)))
  (with-standard-io-syntax (read-from-string (decode (encoding-type-id 'string)))))

;; What we can't do: functions, readtables, restarts.
