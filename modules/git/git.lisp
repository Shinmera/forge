#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.modules.git)

(defmethod forge:ensure-version ((_ (eql :git)))
  (forge:ensure-version (legit:git-describe :tags T)))

(defmethod forge:ensure-version ((_ (eql :git-hash)))
  (forge:parse-version (legit:git-rev-parse "HEAD")))

(defmethod forge:ensure-version ((_ (eql :git-tag)))
  (forge:ensure-version (legit:git-describe :tags T :abbrev "0")))

(defmethod forge:ensure-version ((_ (eql :git-branch)))
  (forge:ensure-version (legit:git-rev-parse "HEAD" :abbrev-ref T)))
