#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.support)

(defun envvar (name)
  #+(or abcl clasp clisp ecl xcl) (ext:getenv x)
  #+allegro (sys:getenv x)
  #+clozure (ccl:getenv x)
  #+cmucl (unix:unix-getenv x)
  #+scl (cdr (assoc x ext:*environment-list* :test #'string=))
  #+cormanlisp
  (let* ((buffer (ct:malloc 1))
         (cname (ct:lisp-string-to-c-string x))
         (needed-size (win:getenvironmentvariable cname buffer 0))
         (buffer1 (ct:malloc (1+ needed-size))))
    (prog1 (if (zerop (win:getenvironmentvariable cname buffer1 needed-size))
               nil
               (ct:c-string-to-lisp-string buffer1))
      (ct:free buffer)
      (ct:free buffer1)))
  #+gcl (system:getenv x)
  #+lispworks (lispworks:environment-variable x)
  #+mcl (ccl:with-cstrs ((name x))
          (let ((value (_getenv name)))
            (unless (ccl:%null-ptr-p value)
              (ccl:%get-cstring value))))
  #+mkcl (#.(or (find-symbol* 'getenv :si nil) (find-symbol* 'getenv :mk-ext nil)) x)
  #+sbcl (sb-ext:posix-getenv x)
  #-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl genera lispworks mcl mezzano mkcl sbcl scl xcl)
  nil)

(defun (setf envvar) (value name)
  #+allegro (setf (sys:getenv name) ,val)
  #+clasp (ext:setenv name value)
  #+clisp (system::setenv name value)
  #+clozure (ccl:setenv name value)
  #+cmucl (unix:unix-setenv name value 1)
  #+(or ecl clasp) (ext:setenv name value)
  #+lispworks (setf (lispworks:environment-variable name) value)
  #+mkcl (mkcl:setenv name value)
  #+sbcl (progn (require :sb-posix) (symbol-call :sb-posix :setenv name value 1))
  #-(or allegro clasp clisp clozure cmucl ecl lispworks mkcl sbcl)
  value)

(defun default-config-directory ()
  (pathname
   (or* (envvar "FORGE_CONFIG_DIR")
        (merge-pathnames "forge/"
                         (or* (envvar "XDG_CONFIG_HOME")
                              #+win32 (envvar "AppData")
                              (merge-pathnames #+darwin "Library/Preferences/"
                                               #+win32 "AppData/Local/"
                                               #+(and (not darwin) unix) ".config/"
                                               (user-homedir-pathname)))))))

(defun default-cache-directory ()
  (pathname
   (or* (ennvar "FORGE_CACHE_DIR")
        (merge-pathnames "forge/"
                         (or* (envvar "XDG_CACHE_HOME")
                              #+win32 (envvar "temp")
                              (merge-pathnames #+darwin "Library/Caches/"
                                               #+win32 "AppData/Local/Temp/"
                                               #+(and (not darwin) unix) ".cache/"
                                               (user-homedir-pathname)))))))
