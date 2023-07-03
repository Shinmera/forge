(in-package #:org.shirakumo.forge.support)

(defun fork ()
  #+sbcl
  (sb-posix:fork)
  #-sbcl
  (error 'implementation-unsupported))

(defun pipe ()
  #+sbcl
  (multiple-value-bind (a b) (sb-posix:pipe)
    (values (sb-sys:make-fd-stream a :input NIL :output t
                                     :element-type '(unsigned-byte 8))
            (sb-sys:make-fd-stream b :input T :output NIL
                                     :element-type '(unsigned-byte 8))))
  #-sbcl
  (error 'implementation-unsupported))
