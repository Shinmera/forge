#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge
  (:use #:cl)
  (:shadow #:step)
  (:local-nicknames
   (#:support #:org.shirakumo.forge.support)
   (#:communication #:org.shirakumo.forge.communication)
   (#:tcp #:org.shirakumo.forge.communication.tcp)
   (#:in-process #:org.shirakumo.forge.communication.in-process)
   (#:promise #:org.shirakumo.promise)
   (#:bt #:bordeaux-threads)
   (#:v #:org.shirakumo.verbose))
  ;; artefact.lisp
  (:export
   #:machine
   #:name
   #:find-registry
   #:artefact-pathname
   #:pathname-artefact
   #:notice-file
   #:registry
   #:name
   #:path
   #:find-artefact
   #:delete-artefact
   #:artefact
   #:path
   #:registry
   #:size
   #:hash
   #:mtime
   #:touch)
  ;; basic.lisp
  (:export
   #:basic-database
   #:parameter-plist-effect
   #:parameter-alist-effect
   #:basic-policy
   #:linear-executor
   #:force
   #:client)
  ;; constraints.lisp
  (:export
   #:constraints-incompatible
   #:a
   #:b
   #:parse-constraint
   #:define-constraint-parser
   #:constraint
   #:version-constraint
   #:version-match-p
   #:constraint-subset-p
   #:unify
   #:widen
   #:unify*
   #:match-constraint
   #:version-unspecific-constraint
   #:version-equal-constraint
   #:version
   #:version-range-constraint
   #:min-version
   #:max-version
   #:constraint-union
   #:constraints)
  ;; network.lisp
  (:export
   #:*server*
   #:no-such-client
   #:name
   #:server
   #:no-such-machine
   #:peer
   #:name
   #:machine
   #:connection
   #:server
   #:on-existing-client
   #:start
   #:stop
   #:list-clients
   #:find-machine
   #:delete-machine
   #:artefact-changed-p
   #:with-promise
   #:client
   #:server
   #:promise-reply
   #:with-client-eval)
  ;; plan.lisp
  (:export
   #:*database*
   #:database
   #:map-effects
   #:list-effects
   #:find-effect
   #:register-effect
   #:do-effects
   #:component
   #:supported-operations
   #:operation
   #:dependencies
   #:perform
   #:make-effect
   #:ensure-effect
   #:dependency
   #:effect-type
   #:parameters
   #:version
   #:hard-p
   #:depend
   #:effect
   #:sources
   #:parameters
   #:add-source
   #:normalize-parameters
   #:variant-p
   #:policy
   #:in-order-to
   #:select-source
   #:select-effect-set
   #:compute-plan
   #:make-operation
   #:executor
   #:plan
   #:first-steps
   #:final-steps
   #:make-step
   #:step
   #:operation
   #:component
   #:effect
   #:predecessors
   #:successors
   #:complete-p
   #:compound-step
   #:inner-effect
   #:execute
   #:effect-realized-p
   #:effect-needed-p
   #:connect)
  ;; version.lisp
  (:export
   #:version
   #:version=
   #:version<
   #:to-string
   #:parse-version
   #:unknown-version
   #:maximal-version
   #:minimal-version
   #:integer-version
   #:value
   #:hashed-version
   #:value
   #:separated-version
   #:value
   #:versioned-object
   #:version))
