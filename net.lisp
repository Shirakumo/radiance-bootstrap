(in-package #:org.shirakumo.radiance.bootstrap.net)

(defun open-connection (host port &key (element-type '(unsigned-byte 8)))
  (feature-case
    (allegro
     (make-socket :remote-host host :remote-port port))
    (abcl
     (let ((socket (make-socket host port)))
       (get-socket-stream socket :element-type element-type)))
    (ccl
     (make-socket :remote-host host :remote-port port))
    ((or clasp ecl sbcl mkcl)
     (let* ((endpoint (host-ent-address (get-host-by-name host)))
            (socket (make-instance 'inet-socket :protocol :tcp :type :stream)))
       (socket-connect socket endpoint port)
       (socket-make-stream socket :element-type element-type
                                  :input T :output T
                                  :buffering :full)))
    (clisp
     (socket-connect port host :element-type element-type))
    ((or cmucl scl)
     (let ((fd (connect-to-inet-socket host port)))
       (make-fd-stream fd :element-type element-type
                          :input T :output T)))
    (lispworks
     (open-tcp-stream host port :element-type element-type
                                :direction :io
                                :errorp T
                                :read-timeout NIL
                                :timeout 5))))

(defun close-connection (connection)
  (ignore-errors (close connection)))

(defun call-with-connection (function host port)
  (let ((connection (open-connection host port)))
    (unwind-protect
         (funcall function connection)
      (close-connection connection))))

(defmacro with-connection ((connection host port) &body body)
  `(call-with-connection (lambda (,connection) ,@body) ,host ,port))

(defun write-connection (buffer connection &key (start 0) end)
  (prog1
      (write-sequence buffer connection :start start :end end)
    (finish-output connection)))

(defun read-connection (buffer connection &key (start 0) end)
  (read-sequence buffer connection :start start :end end))
