(asdf:defsystem forge-module-git
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Git support modules for Forge"
  :homepage "https://shinmera.github.io/forge"
  :bug-tracker "https://github.com/Shinmera/forge/issues"
  :source-control (:git "https://github.com/Shinmera/forge.git")
  :serial T
  :components ((:file "package")
               (:file "git"))
  :depends-on (:forge
               :legit))
