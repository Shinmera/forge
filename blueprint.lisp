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

(defun discover-blueprints (&optional (paths *blueprint-search-paths*))
  (let ((blueprints ()))
    (dolist (path paths blueprints)
      (dolist (path (directory (merge-pathnames "**/blueprint" path)))
        (push path blueprints)))))

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
    (import '(define-project) package)
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
      (let ((blueprint-package (make-blueprint-package))
            (*blueprint-truename* path)
            (*package* blueprint-package)
            (*read-eval* NIL))
        (unwind-protect (load temp NIL NIL :external-format :utf-8)
          (ignore-errors (delete-package blueprint-package))
          (ignore-errors (delete-file temp)))))
    ;; Now that we successfully loaded, use the cached file properties.
    (setf (gethash path *blueprint-timestamp-cache*)
          (list date (hash-file temp)))
    path))
