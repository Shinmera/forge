#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge.support
  (:use #:cl)
  ;; toolkit.lisp
  (:export
   #:forge-condition
   #:define-condition*
   #:argument-missing
   #:arg!
   #:implementation-unsupported
   #:with-retry-restart
   #:try-files
   #:or*
   #:call
   #:prototype
   #:generic<)
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
