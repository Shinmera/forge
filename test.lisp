#|
This file is a part of forge
(c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(rename-package *package* (package-name *package*) '(forge))
(unless (boundp '*database*)
  (setf *database* (make-instance 'basic-database)))
(setf (v:repl-level) :trace)
(start T)

(setf (find-registry :cache *server*) #p "~/.cache/forge/")
(setf (find-registry :test *server*) #p "~/Projects/cl/forge/test/")

(find-artefact "a.lisp" *server* :registry :test :if-does-not-exist :create)

#++
(progn
  (make-instance 'org.shirakumo.forge.modules.lisp::file :file "a.lisp" :project :test)
  (defun test ()
    (org.shirakumo.forge.client:start :machine :server :dedicate NIL)
    (unwind-protect
         (org.shirakumo.forge.client:request-effect "ORG.SHIRAKUMO.FORGE.MODULES.LISP:LOAD-EFFECT" "a.lisp")
      (org.shirakumo.forge.client:stop))))
