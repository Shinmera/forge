#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defun prototype (thing)
  (etypecase thing
    (symbol
     (c2mop:class-prototype (c2mop:ensure-finalized (find-class thing))))
    (class
     (c2mop:class-prototype (c2mop:ensure-finalized thing)))
    (object
     thing)))

(defun pophash (key table &optional default)
  (let ((value (gethash key table default)))
    (remhash key table)
    value))
