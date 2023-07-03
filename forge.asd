(asdf:defsystem forge
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://shinmera.github.io/forge"
  :bug-tracker "https://github.com/Shinmera/forge/issues"
  :source-control (:git "https://github.com/Shinmera/forge.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "tcp")
               (:file "version")
               (:file "constraints")
               (:file "network")
               (:file "plan")
               (:file "module")
               (:file "blueprint")
               (:file "project")
               (:file "basic")
               (:file "dot"))
  :depends-on (:forge-support
               :forge-communication
               :closer-mop
               :promise
               :cl-ppcre
               :usocket
               :documentation-utils
               :pathname-utils
               :bordeaux-threads
               :verbose
               :alexandria
               :ironclad
               :cffi))
