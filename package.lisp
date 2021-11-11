#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.forge
  (:use #:cl)
  (:shadow #:step #:*modules*)
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
   #:client
   #:artefact-effect
   #:artefact-component
   #:artefact
   #:artefact-output-operation
   #:output-artefact
   #:realize-artefact
   #:artefact-input-operation
   #:input-artefact
   #:ensure-artefact-operation
   #:compiler-operation
   #:compiler
   #:target-platform
   #:select-compiler
   #:compiler
   #:name
   #:cache-directory
   #:compiler-output-operation
   #:output-file-type
   #:compiler-input-operation
   #:input-file-type
   #:artefact-project
   #:registry
   #:parent-component
   #:children
   #:child-component
   #:parent
   #:dependencies-component
   #:depends-on
   #:normalize-dependency-spec)
  ;; blueprint.lisp
  (:export
   #:*blueprint-truename*
   #:*blueprint-search-paths*
   #:add-blueprint-search-path
   #:discover-blueprints
   #:load-blueprints
   #:reload-blueprints
   #:maybe-load-blueprint
   #:load-blueprint
   #:list-blueprints)
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
  ;; module.lisp
  (:export
   #:module-entry-point-search-function
   #:define-module-entry-point-search-function
   #:find-module-entry-point
   #:module
   #:load-module
   #:find-module
   #:register-module
   #:list-modules
   #:define-module)
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
   #:dependency-cycle-detected
   #:effect
   #:unsatisfiable-dependency
   #:dependency
   #:operation
   #:component
   #:unsatisfiable-effect
   #:effect
   #:*database*
   #:database
   #:map-effects
   #:list-effects
   #:find-effect
   #:register-effect
   #:do-effects
   #:component
   #:name
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
   #:retry
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
  ;; project.lisp
  (:export
   #:project
   #:blueprint
   #:metadata
   #:in-order-to
   #:ensure-version
   #:find-project
   #:register-project
   #:delete-project
   #:build
   #:normalize-component-spec
   #:parse-component
   #:default-component-type
   #:default-project-type
   #:list-projects
   #:define-project
   #:forge)
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
   #:compound-version
   #:versions
   #:version-from-string
   #:versioned-object
   #:version))

(defpackage #:org.shirakumo.forge.user
  (:use #:cl)
  (:local-nicknames
   (#:forge #:org.shirakumo.forge)))
