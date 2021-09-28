#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.communication)

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
       (declare (type (unsigned-byte ,size) int))
       (declare (type stream stream))
       (declare (optimize speed (safety 1)))
       ,@(loop for i from 0 below size by 8
               collect `(write-byte (ldb (byte 8 ,i) int) stream)))
     (defun ,(intern (format NIL "RU~d" size)) (stream)
       (declare (type stream stream))
       (declare (optimize speed (safety 1)))
       (the (unsigned-byte ,size)
            (+ ,@(loop for i from 0 below size by 8
                       collect `(ash (the (unsigned-byte 8) (read-byte stream)) ,i)))))
     (defun ,(intern (format NIL "RI~d" size)) (stream)
       (declare (type stream stream))
       (declare (optimize speed (safety 1)))
       (let ((bits (the (unsigned-byte ,size)
                        (+ ,@(loop for i from 0 below size by 8
                                   collect `(ash (the (unsigned-byte 8) (read-byte stream)) ,i))))))
         (declare (type (unsigned-byte ,size) bits))
         (the (signed-byte ,size)
              (dpb bits (byte ,size 0)
                   (if (logbitp ,(1- size) bits) -1 0)))))))

;; FIXME: circularity, references

(define-binary-accessor 8)
(define-binary-accessor 16)
(define-binary-accessor 32)
(define-binary-accessor 64)

(defmethod encode-message (value (pathname pathname))
  (with-open-file (stream pathname :direction :output :element-type '(unsigned-byte 8)
                                   :if-exists :supersede)
    (encode-message value stream)))

(defmethod decode-message (type (pathname pathname))
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (decode-message type stream)))

(defmethod decode-message ((type (eql T)) (stream stream))
  (decode-message (ru16 stream) stream))

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

(defmacro define-slot-coder (class slots)
  `(define-encoding ,class (value stream)
     (progn
       ,@(loop for slot in slots
               collect `(encode (slot-value value ',slot))))
     (let ((value (allocate-instance (find-class ',class))))
       ,@(loop for slot in slots
               collect `(setf (slot-value value ',slot) (decode)))
       value)))

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
        (or (let ((maybe-package (find-package package)))
              (when maybe-package
                (find-symbol name maybe-package)))
            (make-dummy-symbol package name)))))

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

(macrolet ((stringgen ()
             (let ((base-id (ensure-encoding-type-id 'base-string))
                   (gen-id (ensure-encoding-type-id 'string)))
               `(progn
                  (defmethod encode-message ((value string) (stream stream))
                    (etypecase value
                      (base-string
                       (wu16 ,base-id stream)
                       (wu32 (length value) stream)
                       (loop for element across value
                             do (wu8 (char-code element) stream)))
                      (string
                       (wu16 ,gen-id stream)
                       (wu32 (length value) stream)
                       (loop for element across value
                             do (wu32 (char-code element) stream)))))
                  (defmethod decode-message ((id (eql ,base-id)) (stream stream))
                    (let ((arr (make-array (ru32 stream) :element-type 'base-char)))
                      (dotimes (i (length arr) arr)
                        (setf (aref arr i) (code-char (ru8 stream))))
                      arr))
                  (defmethod decode-message ((id (eql ,gen-id)) (stream stream))
                    (let ((arr (make-array (ru32 stream) :element-type 'character)))
                      (dotimes (i (length arr) arr)
                        (setf (aref arr i) (code-char (ru32 stream))))))))))
  (stringgen))

;; FIXME: compressed bit vector support
;; FIXME: fill-pointer support
(macrolet ((vecgen ()
             (let ((ub8-id (ensure-encoding-type-id 'ub8-vector))
                   (gen-id (ensure-encoding-type-id 'vector)))
               `(progn
                  (defmethod encode-message ((value vector) (stream stream))
                    (etypecase value
                      ((vector (unsigned-byte 8))
                       (wu16 ,ub8-id stream)
                       (wu32 (length value) stream)
                       (loop for element across value
                             do (wu8 element stream)))
                      (vector
                       (wu16 ,gen-id stream)
                       (wu32 (length value) stream)
                       (loop for object across value
                             do (encode-message object stream)))))
                  (defmethod decode-message ((id (eql ,ub8-id)) (stream stream))
                    (let ((arr (make-array (ru32 stream) :element-type '(unsigned-byte 8))))
                      (read-sequence arr stream)
                      arr))
                  (defmethod decode-message ((id (eql ,gen-id)) (stream stream))
                    (let ((arr (make-array (ru32 stream))))
                      (dotimes (i (length arr) arr)
                        (setf (aref arr i) (decode-message (ru16 stream) stream)))))))))
  (vecgen))

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

;; What we can't do: functions, readtables, restarts, streams.

(define-slot-coder ok (id))
(define-slot-coder exit (id))
(define-slot-coder connect (id machine client-id version))
(define-slot-coder ping (id clock))
(define-slot-coder pong (id clock))
(define-slot-coder error-message (id condition-type arguments report))
(define-slot-coder warning-message (id condition-type arguments report))
(define-slot-coder eval-request (id form))
(define-slot-coder return-message (id value))
(define-slot-coder effect-request (effect-type parameters version execute-on))

(define-encoding artefact (value stream)
  (progn
    (encode* (artefact-target value))
    (with-open-file (input (artefact-source value) :direction :input
                                                   :element-type '(unsigned-byte 8))
      (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
        (declare (dynamic-extent buffer))
        (loop for read = (read-sequence buffer input)
              while (< 0 read)
              do (write-sequence buffer stream :end read)))))
  (let ((target (decode (encoding-type-id 'string))))
    (with-open-file (output target :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-exists :supersede)
      (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
        (declare (dynamic-extent buffer))
        (loop for read = (read-sequence buffer stream)
              while (< 0 read)
              do (write-sequence buffer output :end read))))))

;; Flex to make dummy-symbols appear as symbols on the wire, as the symbol
;; decode takes care of restructuring them as dummies if not found.
(defmethod encode-message ((value dummy-symbol) (stream stream))
  (wu16 #.(encoding-type-id 'symbol) stream)
  (encode-payload value stream))

(defmethod encode-payload ((value dummy-symbol) (stream stream))
  (encode-payload (dummy-symbol-package value) stream)
  (encode-payload (dummy-symbol-name value) stream))

(define-encoding dummy-object (value stream)
  (encode* (dummy-object-description value))
  (make-dummy-object (decode (encoding-type-id 'string))))

(defmethod encode-message (object (stream stream))
  (wu16 #.(encoding-type-id 'dummy-object) stream)
  (encode-payload object stream))

(defmethod encode-payload (object (stream stream))
  (encode-payload (princ-to-string object) stream))
