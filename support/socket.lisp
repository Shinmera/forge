(in-package #:org.shirakumo.forge.support)

(defun open-tcp (host port &key timeout (element-type '(unsigned-byte 8)))
  ;; FIXME: handle timeout
  #+allegro
  (excl:make-socket :remote-host host :remote-port port)
  #+abcl
  (let ((socket (system:make-socket host port)))
    (system:get-socket-stream socket :element-type element-type))
  #+ccl
  (ccl:make-socket :remote-host host :remote-port port)
  #+(or clasp ecl sbcl mkcl)
  (let* ((endpoint (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host)))
         (socket (make-instance 'sb-bsd-sockets:inet-socket :protocol :tcp :type :stream)))
    (sb-bsd-sockets:socket-connect socket endpoint port)
    (sb-bsd-sockets:socket-make-stream socket
                                       :element-type element-type
                                       :input T :output T
                                       :buffering :full))
  #+clisp
  (socket:socket-connect port host :element-type element-type)
  #+(or cmucl scl)
  (let ((fd (extensions:connect-to-inet-socket host port)))
    (extensions:make-fd-stream fd :element-type element-type
                                  :input T :output T))
  #+lispworks
  (comm:open-tcp-stream host port :element-type element-type
                                  :direction :io
                                  :errorp T
                                  :read-timeout NIL
                                  :timeout 5)
  #-(or allegro abcl ccl clasp ecl sbcl mkcl clisp cmucl scl lispworks)
  (error 'implementation-unsupported))
