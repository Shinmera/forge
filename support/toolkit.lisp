#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.support)

(define-condition forge-condition (condition)
  ())

(defgeneric arguments (condition)
  (:method-combination append))

(defmethod arguments append ((condition condition))
  ())

(defmacro define-condition* (name superclasses slots report)
  (let ((slots (loop for slot in slots
                     collect (if (listp slot)
                                 slot
                                 (list slot :initarg (intern (string slot) "KEYWORD")
                                            :initform `(arg! ,(intern (string slot) "KEYWORD"))
                                            :reader slot)))))
    `(progn
       (define-condition ,name (,@superclasses forge-condition)
         ,slots
         (:report (lambda (c s)
                    (declare (ignorable c))
                    (format s ,(first report)
                            ,@(loop for arg in (rest report)
                                    collect (if (symbolp arg)
                                                `(,arg c)
                                                `((lambda (condition) ,arg) c)))))))
       (defmethod arguments append ((,name ,name))
         (list ,@(loop for (slot . args) in slots
                       for reader = (getf args :reader)
                       for initarg = (getf args :initarg)
                       when initarg collect initarg
                       when initarg collect (if reader
                                                `(,reader ,name)
                                                `(slot-value ,name ',slot))))))))

(define-condition* argument-missing (error)
  (argument) ("The argument~%  ~s~%was required, but not given." argument))

(defun arg! (argument)
  (error 'argument-missing :argument argument))

(define-condition* implementation-unsupported (error)
  () ("Your implementation is not supported or does not support a required feature for Forge."))

(defmacro with-retry-restart ((&optional (name 'retry) (format-string "Retry") &rest format-args) &body body)
  (let ((block (gensym "BLOCK"))
        (retry (gensym "RETRY")))
    `(block ,block
       (tagbody
          ,retry
          (restart-case
              (return-from ,block ,@body)
            (,name ()
              :report (lambda (s) (format s ,format-string ,@format-args))
              (go ,retry)))))))

(defun try-files (&rest pathnames)
  (loop for path in pathnames
        do (when (probe-file path) (return path))
        finally (error "No matching paths.")))

(defmacro or* (&rest args)
  (let ((v (gensym "VALUE")))
    `(or ,@(loop for arg in args
                 collect `(let ((,v ,arg))
                            (when (and ,v (not (equal ,v "")))
                              ,v))))))

(defun call (package name &rest args)
  (apply (or (find-symbol (string name) package)
             (error "No symbol named ~s found in ~s." name package))
         args))

(defgeneric generic< (a b)
  (:method ((a real) (b real))
    (< a b))
  (:method ((a string) (b string))
    (string< a b))
  (:method ((a pathname) (b pathname))
    (string< (namestring a) (namestring b)))
  (:method ((a symbol) (b symbol))
    (cond ((eq (symbol-package a) (symbol-package b))
           (string< (symbol-name a) (symbol-name b)))
          ((null (symbol-package b))
           T)
          ((null (symbol-package a))
           NIL)
          (T
           (string< (package-name (symbol-package a)) (package-name (symbol-package b)))))))
