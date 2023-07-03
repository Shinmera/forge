(asdf:defsystem forge-support
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Support functions for Forge to run on multiple implementations."
  :homepage "https://shinmera.github.io/forge"
  :bug-tracker "https://github.com/Shinmera/forge/issues"
  :source-control (:git "https://github.com/Shinmera/forge.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "process")
               (:file "socket")
               (:file "environment"))
  :depends-on ((:feature :allegro (:require :sock))
               (:feature :clasp (:require :sockets))
               (:feature :ecl (:require :sockets))
               (:feature :lispworks (:require "comm"))
               (:feature :mkcl (:require :sockets))
               (:feature :sbcl (:require :sb-bsd-sockets))
               (:feature :sbcl (:require :sb-posix))))
