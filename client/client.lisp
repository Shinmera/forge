#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.client)

(defun forge-package-p (package)
  (flet ((match (name)
           (string= #1="ORG.SHIRAKUMO.FORGE" name :end2 (min (length name) (length #1#)))))
    (or (match (package-name package))
        (some #'match (package-nicknames package)))))

(defun prune-package (package)
  (do-symbols (symbol package (delete-package package))
    (when (eq (symbol-package symbol) package)
      (makunbound symbol)
      (fmakunbound symbol)
      (when (find-class symbol)
        (setf (find-class symbol) NIL)))))

(defun prune ()
  (kill-server)
  (prune-package (remove-if-not #'forge-package-p (list-all-packages))))

(defun ensure-effect-type (effect-type)
  (etypecase effect-type
    (symbol effect-type)
    (communication:dummy-symbol effect-type)
    (string (let ((colon (position #\: effect-type)))
              (communication:make-dummy-symbol
               (subseq effect-type 0 colon) (subseq effect-type (1+ colon)))))
    (list (communication:make-dummy-symbol
           (first effect-type) (second effect-type)))))

(defun request-effect (effect-type parameters &key (version T) (execute-on :self) (connection *connection*))
  (unless connection
    (setf connection (start :dedicate NIL)))
  (let ((message (make-instance 'communication:effect-request
                                :effect-type (ensure-effect-type effect-type)
                                :parameters parameters
                                :version version
                                :execute-on execute-on)))
    (communication:send message connection)
    (command-loop connection :until (communication:id message))))

(defun load-project (project &key (version T) (connection *connection*))
  (request-effect (communication:make-dummy-symbol "ORG.SHIRAKUMO.FORGE.MODULES.LISP" "LOAD-EFFECT")
                  (list :project project) :version version :connection connection))
