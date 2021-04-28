#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.protocol)

(defvar *encoding-type-id-map* (make-hash-table :test 'eq))
(defvar *encoding-type-id-counter* 0)

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

(define-binary-accessor 8)
(define-binary-accessor 16)
(define-binary-accessor 32)
(define-binary-accessor 64)

(defun ensure-encoding-type-id (type)
  (or (gethash type *encoding-type-id-map*)
      (setf (gethash type *encoding-type-id-map*)
            (incf *encoding-type-id-counter*))))

(defun encoding-type-id (type)
  (or (gethash type *encoding-type-id-map*)
      (error "The type ~s is not assigned an ID." type)))

(defun (setf encoding-type-id) (id type)
  (setf (gethash type *encoding-type-id-map*) id))

(defmacro define-encoding (type (value stream) &body en/de)
  (destructuring-bind (encoder decoder) en/de
    (let ((id (ensure-encoding-type-id type)))
      `(flet ((encode (,value ,stream)
                (flet ((encode (,value)
                         (encode-message ,value ,stream))
                       (encode* (,value)
                         (encode-payload ,value ,stream)))
                  ,encoder)))
         (setf (encoding-type-id ',type) ,id)
         (defmethod encode-message ((,value ,type) ,stream)
           (wu16 ,id ,stream)
           (encode ,value ,stream))
         (defmethod encode-payload ((,value ,type) ,stream)
           (encode ,value ,stream))
         (defmethod decode-message ((id (eql ,id)) ,stream)
           (flet ((decode (&optional (,value (ru16 ,stream)))
                    (decode-message ,value ,stream)))
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

(define-encoding integer (value stream)
  (progn
    (check-type value (signed-byte 64))
    (wu64 value stream))
  (ri64 stream))

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

(define-encoding string (value stream)
  (progn (wu32 (length value) stream)
         ;; FIXME: convert to utf-8
         (loop for char across value
               do (wu32 (char-code char) stream)))
  (let ((arr (make-array (ru32 stream) :element-type 'character)))
    (dotimes (i (length arr) arr)
      (setf (aref arr i) (ru32 stream)))))

;; KLUDGE: Pack element type
(define-encoding vector (value stream)
  (progn (wu32 (length value) stream)
         (loop for object across value
               do (encode object)))
  (let ((arr (make-array (ru32 stream))))
    (dotimes (i (length arr) arr)
      (setf (aref arr i) (decode)))))

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

#++
(define-encoding pathname (value stream)
  )
