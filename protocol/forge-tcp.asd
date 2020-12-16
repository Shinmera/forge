#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem forge-tcp
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Tiny TCP compatibility library."
  :homepage "https://shinmera.github.io/forge"
  :bug-tracker "https://github.com/Shinmera/forge/issues"
  :source-control (:git "https://github.com/Shinmera/forge.git")
  :serial T
  :components ((:file "tcp"))
  :depends-on (:forge-protocol
               (:feature :allegro (:require :sock))
               (:feature :clasp (:require :sockets))
               (:feature :ecl (:require :sockets))
               (:feature :lispworks (:require "comm"))
               (:feature :mkcl (:require :sockets))
               (:feature :sbcl (:require :sb-bsd-sockets))))
