#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.support)

(defun fork ()
  #+sbcl
  (sb-posix:fork)
  #-sbcl
  (error "Forking unsupported"))

(defun pipe ()
  #+sbcl
  (multiple-value-bind (a b) (sb-posix:pipe)
    (values (sb-sys:make-fd-stream a :input NIL :output t
                                     :element-type '(unsigned-byte 8))
            (sb-sys:make-fd-stream b :input T :output NIL
                                     :element-type '(unsigned-byte 8)))))
