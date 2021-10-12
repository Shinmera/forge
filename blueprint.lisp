#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge)

(defvar *blueprint-truename* NIL)
(defvar *blueprint-search-paths* ())
(defvar *blueprint-timestamp-cache* (make-hash-table :test 'equal))

(defun add-blueprint-search-path (path &key discover if-exists)
  (loop for existing in *blueprint-search-paths*
        do (when (pathname-utils:subpath-p path existing)
             (ecase if-exists
               ((NIL) (return-from add-blueprint-search-path NIL))
               (:error (error 'blueprint-search-path-exists :path path :existing existing)))))
  (push path *blueprint-search-paths*)
  (when discover
    (load-blueprints (discover-blueprints (list path))))
  path)

#+linux
(progn ; Much faster scanning on linux using the d_type and direct byte comparisons.
  (cffi:defcstruct (dirent :class dirent :conc-name dirent-)
    (inode :size)
    (offset :size)
    (length :uint16)
    (type :uint8)
    (name :char :count 256))

  (cffi:defcfun strlen :size
    (path :pointer))

  (cffi:defcfun strcmp :int
    (a :pointer)
    (b :pointer))

  (cffi:defcfun strcpy :int
    (dest :pointer)
    (src :pointer))

  (cffi:defcfun opendir :pointer
    (dir :pointer))

  (cffi:defcfun closedir :int
    (dir :pointer))

  (cffi:defcfun readdir :pointer
    (dir :pointer))

  (cffi:defcfun openat :int
    (dir :int)
    (name :pointer)
    (flags :int))

  (cffi:defcfun fdopendir :pointer
    (fd :int))

  (cffi:defcfun readlink :ssize
    (path :string)
    (target :pointer)
    (size :size))

  (defun scan-directory (dir callback)
    (cffi:with-foreign-string (blueprint "blueprint")
      (cffi:with-foreign-object (path :char 4096)
        (cffi:lisp-string-to-foreign dir path 4096)
        (labels ((scan (fd)
                   (let ((handle (fdopendir fd)))
                     (unless (cffi:null-pointer-p handle)
                       (unwind-protect
                            (loop for entry = (readdir handle)
                                  until (cffi:null-pointer-p entry)
                                  do (let* ((name (cffi:foreign-slot-pointer entry '(:struct dirent) 'name))
                                            (namelen (strlen name)))
                                       (when (or (< 2 namelen)
                                                 (and (/= (char-code #\.) (cffi:mem-aref name :char 0))
                                                      (/= (char-code #\.) (cffi:mem-aref name :char 1))))
                                         (flet ((dir (fd)
                                                  (unwind-protect (scan fd)
                                                    (cffi:foreign-funcall "close" :int fd :int)))
                                                (file (fd)
                                                  (when (= 0 (strcmp name blueprint))
                                                    (funcall callback (format NIL "~a/blueprint" (sb-posix:readlink (format NIL "/proc/self/fd/~d" fd)))))))
                                           (case (dirent-type entry)
                                             (0 ; Unknown
                                              (let ((inner (openat fd name 592128)))
                                                (if (= -1 inner)
                                                    (file fd)
                                                    (dir inner))))
                                             (4 ; Directory
                                              (dir (openat fd name 592128)))
                                             (8 ; Regular
                                              (file fd)))))))
                         (closedir handle))))
                   T))
          (let ((fd (cffi:foreign-funcall "open" :pointer path :int 592128 :int)))
            (unless (= -1 fd)
              (scan fd))))))))

#-linux
(defun scan-directory (dir callback)
  (dolist (path (directory (merge-pathnames "**/blueprint" dir)))
    (funcall callback path)))

(defun discover-blueprints (&optional (paths *blueprint-search-paths*))
  (let ((blueprints ()))
    (dolist (path paths blueprints)
      (scan-directory (namestring path) (lambda (path) (push path blueprints))))))

(defun load-blueprints (&optional (paths (discover-blueprints)))
  (loop for path in paths
        for value = (maybe-load-blueprint path)
        when value collect value))

(defun reload-blueprints (&key force)
  (loop for path being the hash-keys of *blueprint-timestamp-cache*
        for result = (cond ((not (probe-file path))
                            (warn 'blueprint-no-longer-exists :path path)
                            NIL)
                           (force
                            (load-blueprint path))
                           (T
                            (maybe-load-blueprint path)))
        when result collect result))

(defun maybe-load-blueprint (path)
  (let ((actual-date (file-write-date path)))
    (destructuring-bind (&optional (cached-date 0) (hash #())) (gethash path *blueprint-timestamp-cache*)
      (when (or (< cached-date actual-date)
                (and (= cached-date actual-date)
                     (not (equal hash (hash-file path)))))
        (load-blueprint path)))))

(defun make-blueprint-package ()
  (let ((package (make-package (format NIL "ORG.SHIRAKUMO.FORGE.BLUEPRINT.~a" (random-id)) :use ())))
    (sb-ext:add-package-local-nickname "FORGE" #.*package* package)
    package))

(defun load-blueprint (path)
  (let* ((path (truename path))
         (temp (tempfile :type "lisp"))
         (date (file-write-date path)))
    ;; We copy the file out to ensure that changes to the original while loading
    ;; don't impact the load and are properly detected as new changes when
    ;; attempting to load the file again.
    (uiop:copy-file path temp)
    ;; FIXME: Instead of LOAD use something like Eclector to read and then
    ;;        selectively evaluate forms
    (with-standard-io-syntax
      (let* ((hash (hash-file temp))
             (blueprint-package (make-blueprint-package))
             (*blueprint-truename* path)
             (*package* blueprint-package)
             (*read-eval* NIL))
        (unwind-protect (load temp :verbose NIL :print NIL :external-format :utf-8)
          (ignore-errors (delete-package blueprint-package))
          (ignore-errors (delete-file temp)))
        ;; Now that we successfully loaded, use the cached file properties.
        (setf (gethash path *blueprint-timestamp-cache*)
              (list date hash))))
    path))

(defun list-blueprints ()
  (loop for file being the hash-keys of *blueprint-timestamp-cache*
        collect file))
