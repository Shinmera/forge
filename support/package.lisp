#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge.support
  (:use #:cl)
  (:export
   #:with-retry-restart
   #:try-files)
  (:export
   #:open-tcp
   #:list-tcp
   #:accept-tcp)
  (:export
   #:launch
   #:terminate
   #:exit-code))
