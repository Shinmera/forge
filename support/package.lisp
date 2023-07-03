(defpackage #:org.shirakumo.forge.support
  (:use #:cl)
  ;; toolkit.lisp
  (:export
   #:*debugger*
   #:forge-condition
   #:arguments
   #:define-condition*
   #:argument-missing
   #:arg!
   #:implementation-unsupported
   #:with-retry-restart
   #:try-files
   #:or*
   #:call
   #:prototype
   #:generic<
   #:handler-case*)
  ;; socket.lisp
  (:export
   #:open-tcp)
  ;; process.lisp
  (:export
   #:launch
   #:terminate
   #:exit-code)
  ;; environment.lisp
  (:export
   #:envvar
   #:default-config-directory
   #:default-cache-directory)
  ;; fork.lisp
  (:export
   #:fork))
