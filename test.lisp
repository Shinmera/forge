(ql:quickload :forge-module-lisp)
(in-package #:org.shirakumo.forge)

(rename-package *package* (package-name *package*) '(forge))
(unless (boundp '*database*)
  (setf *database* (make-instance 'basic-database)))
(setf (v:repl-level) :trace)
(start T :if-exists NIL)

(setf (find-registry :cache *server* :if-exists NIL) #p"~/.cache/forge/")
(add-blueprint-search-path #p"~/Projects/cl/forge/")
(load-blueprints)
(dot (build "test" :executor 'dummy-executor) #p"~/a.png")
(build "test")
