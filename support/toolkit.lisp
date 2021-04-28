#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.support)

(defmacro with-retry-restart ((&optional (name 'retry) (format-string "Retry") &rest format-args) &body body)
  (let ((block (gensym "BLOCK"))
        (retry (gensym "RETRY")))
    `(block ,block
       (tagbody
          ,retry
          (restart-case
              (return-from ,block ,@body)
            (,name ()
              :report (lambda (s) (format s ,format-string ,@format-args))
              (go ,retry)))))))

(defun try-files (&rest pathnames)
  (loop for path in pathnames
        do (when (probe-file path) (return path))
        finally (error "No matching paths.")))
