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
  ;; basic.lisp
  (:export
   #:basic-database
   #:parameter-plist-effect
   #:parameter-alist-effect
   #:basic-policy
   #:linear-executor
   #:force
   #:client
   #:file-component
   #:full-path
   #:hash
   #:parent-component
   #:children
   #:child-component
   #:parent
   #:dependencies-component
   #:depends-on
   #:normalize-dependency-spec)
  ;; blueprint.lisp
  (:export
   #:*blueprint*
   #:*blueprint-search-paths*
   #:blueprint
   #:path
   #:projects
   #:list-blueprints
   #:load-blueprint-file
   #:load-blueprint
   #:add-blueprint-search-path
   #:discover-blueprint-files
   #:load-blueprints
   #:reload-blueprints
   #:handle-blueprint-form)
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
   #:on-client-connect
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
   #:compiler
   #:name
   #:cache-directory
   #:policy
   #:compiler
   #:in-order-to
   #:select-source
   #:select-effect-set
   #:compute-plan
   #:retry
   #:make-operation
   #:select-compiler
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
   #:forced-p
   #:compound-step
   #:inner-effect
   #:execute
   #:effect-needed-p
   #:step-needed-p
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
