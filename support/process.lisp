(in-package #:org.shirakumo.forge.support)

(defun %launch (program args &key (output *standard-output*) (error *error-output*))
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
    #+sbcl (call sb-ext:run-program program args)
    #-(or abcl allegro clozure cmucl ecl lispworks mkcl sbcl)
    (error 'implementation-unsupported)))

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
  #+mkcl (nth-value 1 (%launch program args))
  #-(or abcl allegro clozure cmucl ecl lispworks mkcl sbcl)
  (error 'implementation-unsupported))

(defun terminate (process)
  #+abcl (sys:process-kill process)
  #+(and allegro unix) (excl.osi:kill process 15)
  #+(and clozure unix) (ccl:signal-external-process process 15)
  #+(and cmucl unix) (ext:process-kill process 15)
  #+ecl (ext:terminate-process process)
  #+lispworks7+ (sys:pipe-kill-process process)
  #+mkcl (mk-ext:terminate-process process)
  #+(and sbcl unix) (sb-ext:process-kill process 15)
  #-(or abcl allegro clozure cmucl ecl lispworks mkcl sbcl)
  (error 'implementation-unsupported))

(defun exit-code (process)
  #+abcl (unless (sys:process-alive-p process) 0)
  #+allegro (sys:reap-os-subprocess :pid process :wait NIL)
  #+clozure (nth-value 1 (ccl:external-process-status process))
  #+cmucl (let ((status (ext:process-status process)))
            (when (member status '(:exited :signaled))
              (ext:process-exit-code process)))
  #+ecl (nth-value 1 (ext:external-process-status process))
  #+lispworks
  #+lispworks7+ (sys:pipe-exit-status process :wait NIL)
  #-lispworks7+ (sys:pid-exit-status process :wait NIL)
  #+mkcl (let ((status (mk-ext:process-status process)))
           (when (eq status :exited)
             (mk-ext:process-exit-code process)))
  #+sbcl (let ((status (sb-ext:process-status process)))
           (unless (eq status :running)
             (sb-ext:process-exit-code process)))
  #-(or abcl allegro clozure cmucl ecl lispworks mkcl sbcl)
  (error 'implementation-unsupported))
