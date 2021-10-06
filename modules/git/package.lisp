#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge.modules.git
  (:use #:cl)
  (:local-nicknames
   (#:support #:org.shirakumo.forge.support)
   (#:communication #:org.shirakumo.forge.communication)
   (#:forge #:org.shirakumo.forge)
   (#:promise #:org.shirakumo.promise))
  (:export))
