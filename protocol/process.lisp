#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge.protocol.process
  (:use #:cl)
  (:local-nicknames
   (#:protocol #:org.shirakumo.forge.protocol))
  (:export
   #:launch))

(in-package #:org.shirakumo.forge.protocol.process)

(defun %launch (program args)
  (macrolet ((call (fun &rest args)
               `(,fun ,@args :output output
                             :error-output error
                             :error error
                             :if-output-exists :append
                             :if-error-output-exists :append
                             :if-error-exists :append
                             :wait NIL
                             :element-type 'character
                             :external-format :utf-8
                             :save-exit-status T
                             :allow-other-keys T)))
    #+abcl (call sys:run-program program args)
    #+allegro (call excl:run-shell-command (coerce (list* program args) 'vector))
    #+clozure (call ccl:run-program program args)
    #+(or cmucl ecl) (call ext:run-program program args)
    #+lispworks (call system:run-shell-command (list* program args))
    #+mkcl (call mk-ext:run-program program args)
    #+sbcl (call sb-ext:run-program program args)))

(defun launch (program args)
  #+(or abcl clozure cmucl sbcl) (%launch program args)
  #+allegro
  (multiple-value-bind (in-or-io out-or-err err-or-pid pid-or-nil) (%launch program args)
    (declare (ignore in-or-io out-or-err))
    (or pid-or-nill err-or-pid))
  #+ecl (nth-value 2 (%launch program args))
  #+lispworks
  (multiple-value-bind (io-or-pid err-or-nil #-lispworks7+ pid-or-nil) (%launch program args)
    (declare (ignore err-or-nil))
    (or pid-or-nil io-or-pid))
  #+mkcl (nth-value 1 (%launch program args)))

(defun terminate (process)
  #+abcl (sys:process-kill process)
  #+ecl (ext:terminate-process process)
  #+lispworks7+ (sys:pipe-kill-process process)
  #+mkcl (mk-ext:terminate-process process)
  #+unix
  #+allegro (excl.osi:kill process 15)
  #+clozure (ccl:signal-external-process process 15)
  #+cmucl (ext:process-kill process 15)
  #+sbcl (sb-ext:process-kill process 15))

(defun exit-code (process))
