#|
 This file is a part of forge
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.forge.protocol)

(defclass artefact ()
  ((id :initarg :id :reader id)))

(defclass evaluate (command)
  ((form :initarg :form :reader form)))

(defclass check-artefact (command)
  (artefact))

(defclass retrieve-artefact (command)
  (artefact))

(defclass store-artefact (command)
  (artefact))

(defclass query-artefacts (command)
  (query))
