#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.protocol)

(defclass artefact () ())
(defclass side-effect-artefact (artefact) ())
(defclass input-artefact (artefact) (input))
(defclass output-artefact (artefact) (output))
(defclass file-artefact (artefact) ())

(defclass build-artefact (command) (artefact args))
(defclass check-artefact (command) (artefact))
(defclass retrieve-artefact (command) (artefact))
(defclass store-artefact (command) (artefact))
(defclass query-artefacts (command) (query))
