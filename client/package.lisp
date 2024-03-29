(defpackage #:org.shirakumo.forge.client
  (:use #:cl)
  (:shadow #:log)
  (:local-nicknames
   (#:support #:org.shirakumo.forge.support)
   (#:tcp #:org.shirakumo.forge.communication.tcp)
   (#:in-process #:org.shirakumo.forge.communication.in-process)
   (#:communication #:org.shirakumo.forge.communication))
  (:export
   #:start
   #:stop
   #:connected-p
   #:prune
   #:request-effect
   #:load-project))
