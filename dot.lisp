#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defgeneric dot (thing stream))

(defmethod dot (thing (target pathname))
  (if (find (pathname-type target) '("png" "svg" "pdf" "ps" "gif" "jpg") :test #'string=)
      (let ((temp (make-pathname :type "gv" :defaults target)))
        (unwind-protect
             (progn
               (dot thing temp)
               (uiop:run-program (list "dot"
                                       (format NIL "-T~a" (pathname-type target))
                                       (format NIL "-o~a" (namestring (truename target)))
                                       (namestring (truename temp)))
                                 :output T :error-output T))
          (delete-file temp)))
      (with-open-file (stream target :direction :output
                                     :if-exists :supersede)
        (dot thing stream)))
  target)

(defmethod dot (thing (target string))
  (dot thing (pathname target)))

(defmethod dot (thing (target (eql T)))
  (dot thing *standard-output*))

(defmethod dot (thing (target null))
  (with-output-to-string (stream)
    (dot thing stream)))

(defmethod dot ((plan plan) (stream stream))
  (flet ((output (format-string &rest format-args)
           (format stream "~?~%" format-string format-args)))
    (output "digraph plan {")
    (output "  splines=ortho;")
    (let ((visit-cache (make-hash-table :test 'eq))
          (counter 0))
      (labels ((visit (step)
                 (unless (gethash step visit-cache)
                   (let ((count (incf counter)))
                     (setf (gethash step visit-cache) count)
                     (output "  s~d [shape=box,label=~s];" count (dot step NIL))
                     (loop for successor in (successors step)
                           do (visit successor)
                              (output "  s~d -> s~d;" count (gethash successor visit-cache)))))))
        (loop for step across (first-steps plan)
              do (visit step))))
    (output "}")))

(defmethod dot ((step step) (stream stream))
  (dot (operation step) stream)
  (write-char #\Space stream)
  (dot (component step) stream))

(defmethod dot ((component artefact-component) (stream stream))
  (format stream "~a" (list (registry (artefact component))
                            (path (artefact component)))))

(defmethod dot ((component component) (stream stream))
  (format stream "~a" (type-of component)))

(defmethod dot ((operation operation) (stream stream))
  (format stream "~a" (type-of operation)))
