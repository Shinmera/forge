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

(defun wait-for-thread-exit (thread)
  (loop for i from 0
        do (unless (bt:thread-alive-p thread)
             (return))
           (when (<= 10 i)
             (restart-case (error "Message thread is not shutting down!")
               (interrupt (&optional (function #'break))
                 :report "Try to interrupt the thread."
                 (bt:interrupt-thread thread function))
               (abort ()
                 :report "Kill and forget the thread."
                 (bt:destroy-thread thread))
               (continue ()
                 :report "Continue waiting.")))
           (sleep 0.1)))

(defmacro with-event-loop (bindings &body body)
  (let ((last-check (gensym "LAST-CHECK")))
    `(let ((,last-check (get-internal-real-time))
           ,@bindings)
       (loop ,@body
             ;; Backoff to make sure we don't overheat
             (let* ((new-time (get-internal-real-time))
                    (seconds-passed (/ (- new-time ,last-check) internal-time-units-per-second)))
               (when (< seconds-passed 0.01)
                 (sleep (- 0.01 seconds-passed)))
               (setf ,last-check new-time))))))

(defmacro with-retry ((&optional (restart-report "Retry the operation.")) &body body)
  (let ((retry (gensym "RETRY")))
    `(block NIL
       (tagbody
          ,retry
          (flet ((retry ()
                   (go ,retry)))
            (restart-case
                (return
                  (progn
                    ,@body))
              (retry ()
                :report ,restart-report
                (retry))))))))

(defmacro with-cleanup-on-unwind (cleanup &body body)
  (let ((clean (gensym "CLEAN")))
    `(let ((,clean NIL))
       (unwind-protect
            (multiple-value-prog1
                (progn ,@body)
              (setf ,clean T))
         (unless ,clean
           ,cleanup)))))

(defmacro define-print-object-method (class (instance stream &key identity) &body body)
  `(defmethod print-object ((,instance ,class) ,stream)
     (print-unreadable-object (,instance ,stream :type T :identity ,identity)
       ,@body)))

(defmacro define-print-object-method* (class format-string &rest args)
  `(define-print-object-method ,class (,class stream)
     (format stream ,format-string ,@(loop for arg in args
                                           collect (if (listp arg) arg `(,arg ,class))))))

(defun unsafe-path-char-p (char)
  (or (find char "/\\<>:|?*\"")
      (<= 0 (char-code char) 31)))

(defun tempdir ()
  (pathname
   (format NIL "~a/"
           #+windows
           (or (support:envvar "TEMP")
               "~/AppData/Local/Temp")
           #+darwin
           (or (support:envvar "TMPDIR")
               "/tmp")
           #+linux
           (or (support:envvar "XDG_RUNTIME_DIR")
               "/tmp")
           #-(or windows darwin linux)
           "/tmp")))

(defun random-id ()
  (format NIL "~8,'0x-~8,'0x" (random #xFFFFFFFF) (get-universal-time)))

(defun tempfile (&key name type)
  (loop for path = (make-pathname :name (or name (random-id))
                                  :type (or type "tmp")
                                  :defaults (tempdir))
        do (unless (probe-file path) (return path))))

(defun hash-file (file)
  (etypecase file
    ((or string pathname)
     (ironclad:digest-file :sha256 file))
    (stream
     (ironclad:digest-stream :sha256 file))))

(defun removef (plist &rest fields)
  (loop for (k v) on plist by #'cddr
        for found = (find k fields)
        unless found collect k
        unless found collect v))

(defun ensure-instance (designator type &optional default)
  (cond ((null designator)
         (if default
             (make-instance default)
             (error "Need a~%  ~s" type)))
        ((typep designator 'symbol)
         (ensure-instance (make-instance designator) type))
        ((typep designator type)
         designator)
        (T
         (error "Don't know what to do with~%  ~s" designator))))

(defun enlist (a &rest args)
  (if (listp a) a (list* a args)))

(defun delist (a &optional (n 0))
  (if (listp a) (nth n a) a))


#+linux
(progn ; Much faster scanning on linux using the d_type and direct byte comparisons.
  (cffi:defcstruct (dirent :class dirent :conc-name dirent-)
    (inode :size)
    (offset :size)
    (length :uint16)
    (type :uint8)
    (name :char :count 256))

  (defun scan-directory (dir filename callback)
    (cffi:with-foreign-string (blueprint filename)
      (cffi:with-foreign-object (path :char 4096)
        (cffi:lisp-string-to-foreign (pathname-utils:native-namestring dir) path 4096)
        (labels ((scan (fd)
                   (let ((handle (cffi:foreign-funcall "fdopendir" :int fd :pointer)))
                     (unless (cffi:null-pointer-p handle)
                       (unwind-protect
                            (loop for entry = (cffi:foreign-funcall "readdir" :pointer handle :pointer)
                                  until (cffi:null-pointer-p entry)
                                  do (let* ((name (cffi:foreign-slot-pointer entry '(:struct dirent) 'name))
                                            (namelen (cffi:foreign-funcall "strlen" :pointer name :size)))
                                       (when (or (< 2 namelen)
                                                 (and (/= (char-code #\.) (cffi:mem-aref name :char 0))
                                                      (/= (char-code #\.) (cffi:mem-aref name :char 1))))
                                         (flet ((dir (fd)
                                                  (unwind-protect (scan fd)
                                                    (cffi:foreign-funcall "close" :int fd :int)))
                                                (file (fd)
                                                  (when (= 0 (cffi:foreign-funcall "strcmp" :pointer name :pointer blueprint :int))
                                                    (funcall callback (format NIL "~a/~a"
                                                                              (sb-posix:readlink (format NIL "/proc/self/fd/~d" fd))
                                                                              filename)))))
                                           (case (dirent-type entry)
                                             (0 ; Unknown
                                              (let ((inner (cffi:foreign-funcall "openat" :int fd :pointer name :int 592128 :int)))
                                                (if (= -1 inner)
                                                    (file fd)
                                                    (dir inner))))
                                             (4 ; Directory
                                              (dir (cffi:foreign-funcall "openat" :int fd :pointer name :int 592128 :int)))
                                             (8 ; Regular
                                              (file fd)))))))
                         (cffi:foreign-funcall "closedir" :pointer handle :int))))
                   T))
          (let ((fd (cffi:foreign-funcall "open" :pointer path :int 592128 :int)))
            (unless (= -1 fd)
              (scan fd))))))))

#-linux
(defun scan-directory (dir filename callback)
  (dolist (path (directory (merge-pathnames (merge-pathnames filename "**/") dir)))
    (funcall callback path)))
