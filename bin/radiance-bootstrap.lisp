(in-package #:cl-user)
(defpackage #:org.shirakumo.radiance.bootstrap.impl
  (:nicknames #:rb-impl)
  (:use #:cl)
  (:export #:featurep
           #:feature-case))

(defpackage #:org.shirakumo.radiance.bootstrap.net
  (:nicknames #:rb-net)
  (:use #:cl #:org.shirakumo.radiance.bootstrap.impl)
  (:export #:open-connection
           #:close-connection
           #:with-connection
           #:read-connection
           #:write-connection))

(defpackage #:org.shirakumo.radiance.bootstrap.url
  (:nicknames #:rb-url)
  (:use #:cl)
  (:export #:destructure-url))

(defpackage #:org.shirakumo.radiance.bootstrap.http
  (:nicknames #:rb-http)
  (:use #:cl
        #:org.shirakumo.radiance.bootstrap.net
        #:org.shirakumo.radiance.bootstrap.url)
  (:export #:open-request
           #:with-request
           #:write-request-body
           #:download-text
           #:download-file))

(defpackage #:org.shirakumo.radiance.bootstrap
  (:nicknames #:rb)
  (:use #:cl #:org.shirakumo.radiance.bootstrap.http)
  (:export #:bootstrap
           #:install))
(in-package #:org.shirakumo.radiance.bootstrap.impl)

(defun featurep (feature)
  (etypecase feature
    (cons (ecase (first feature)
            (or (loop for subfeature in (rest feature)
                      thereis (featurep subfeature)))
            (and (loop for subfeature in (rest feature)
                       always (featurep subfeature)))
            (not (not (featurep (second feature))))))
    (symbol (find feature *features* :test #'string=))))

(defmacro feature-case (&body cases)
  `(cond ,@(loop for (feature . body) in cases
                 collect (if (eql feature T)
                             `(T ,@body)
                             `((featurep ',feature) ,@body)))))

#+allegro (require :sock)
#+clasp (require :sockets)
#+ecl (require :sockets)
#+lispworks (require "comm")
#+mkcl (require :sockets)
#+sbcl (require :sb-bsd-sockets)

(defun impl-export (source-packages symbol-names)
  (loop for name in symbol-names
        for package = (find name source-packages :test #'find-symbol)
        do (if package
               (import (find-symbol name package) '#:rb-impl)
               (intern name '#:rb-impl))
           (export (find-symbol name '#:rb-impl) '#:rb-impl)
        collect (find-symbol name '#:rb-impl)))

(defmacro define-impl-symbols (source-packages &body symbol-names)
  `(impl-export ',(loop for name in source-packages
                        for package = (find-package name)
                        when package collect package)
                ',(mapcar #'string symbol-names)))

(define-impl-symbols (:system :socket :excl :ccl :sb-bsd-sockets :extensions :comm
                              :excl :ccl :ext :unix :si :lw :mkcl :sb-ext)
  ;;; Socket Stuff
  ;; abcl (system)
  #:make-socket #:get-socket-stream
  ;; allegro (socket excl)
  #:make-socket #:delete-directory
  ;; ccl (ccl)
  #:make-socket
  ;; clasp ecl mkcl sbcl (sb-bsd-sockets)
  #:get-host-by-name #:host-ent-address #:socket-connect #:socket-make-stream #:inet-socket
  ;; clisp (socket)
  #:socket-connect
  ;; cmucl scl (extensions system)
  #:connect-to-inet-socket
  #:make-fd-stream
  ;; lispworks (comm)
  #:open-tcp-stream #:get-host-entry
  ;;; Directory stuff
  ;; allegro (excl)
  #:delete-directory
  ;; ccl (ccl)
  #:delete-empty-directory
  ;; clisp (ext)
  #:delete-directory
  ;; cmucl scl (unix)
  #:unix-rmdir
  ;; clasp ecl (si)
  #:rmdir
  ;; lispworks (lw)
  #:delete-directory
  ;; mkcl (mkcl)
  #:rmdir
  ;; sbcl (sb-ext)
  #:delete-directory)
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
(in-package #:org.shirakumo.radiance.bootstrap.url)

(defun read-url-schema (in)
  (with-output-to-string (out)
    (loop for char = (or (read-char in NIL)
                         (error "Incomplete schema part."))
          do (case char
               (#\: (if (and (char= #\/ (read-char in))
                             (char= #\/ (read-char in)))
                        (return)
                        (error "Malformed schema.")))
               (T (write-char char out))))))

(defun read-url-host (in)
  (with-output-to-string (out)
    (loop for char = (read-char in NIL)
          do (case char
               ((NIL) (return))
               ((#\/ #\:) (unread-char char in) (return))
               (T (write-char char out))))))

(defun read-url-port (in)
  (when (eql #\: (peek-char NIL in NIL))
    (read-char in)
    (parse-integer
     (with-output-to-string (out)
       (loop for char = (read-char in NIL)
             do (case char
                  ((NIL) (return))
                  (#\/ (unread-char char in) (return))
                  (T (write-char char out))))))))

(defun read-url-path (in)
  (if (peek-char NIL in NIL)
      (with-output-to-string (out)
        (loop for char = (read-char in NIL)
              while char
              do (write-char char out)))
      "/"))

(defun destructure-url (url)
  (with-input-from-string (in url)
    (values (read-url-schema in)
            (read-url-host in)
            (read-url-port in)
            (read-url-path in))))
(in-package #:org.shirakumo.radiance.bootstrap.http)

(defun acode (name)
  (case name
    (:cr 13) (:lf 10) (:space 32)
    (T (char-code name))))

(defun header-terpri (out)
  (write-byte 13 out)
  (write-byte 10 out))

(defun aformat (out string &rest args)
  (let ((string (apply #'format NIL string args)))
    (loop for char across string
          for code = (char-code char)
          do (if (< code 127)
                 (write-byte code out)
                 (error "Cannot write character ~s; its code ~a lies outside of the ASCII range."
                        char code)))))

(defun write-query-line (connection method path)
  (aformat connection "~:@(~a~) ~a HTTP/1.1" method path)
  (header-terpri connection))

(defun write-headers (connection headers)
  (loop for (key . val) in headers
        do (aformat connection "~a: ~a" key val)
           (header-terpri connection))
  (header-terpri connection))

(defun read-token (connection &optional (end (acode :space)))
  (with-output-to-string (out)
    (block NIL
      (labels ((process-byte (byte)
                 (cond ((= byte end) (return))
                       ((= byte (acode :cr))
                        (let ((next (read-byte connection)))
                          (cond ((= next (acode :lf))
                                 (return))
                                (T (write-char (code-char byte) out)
                                   (process-byte next)))))
                       (T (write-char (code-char byte) out)))))
        (loop for byte = (read-byte connection NIL)
              while byte
              do (process-byte byte))))))

(defun skip-until-crlf (connection)
  (loop until (and (= (read-byte connection) (acode :cr))
                   (= (read-byte connection) (acode :lf)))))

(defun read-response-line (connection)
  (let ((httpvers (read-token connection))
        (statuscode (read-token connection)))
    (skip-until-crlf connection)
    (unless (string= "HTTP/1.1" httpvers)
      (error "Invalid HTTP response line, expected HTTP/1.1, got ~s"
             httpvers))
    (parse-integer statuscode)))

(defun read-header-key (connection)
  (let ((key (string-trim " " (read-token connection (acode #\:)))))
    (when (string/= "" key)
      (read-byte connection))
    key))

(defun read-header-val (connection)
  (read-token connection -1))

(defun read-headers (connection)
  (let ((table (make-hash-table :test 'equal)))
    (loop for key = (read-header-key connection)
          while (string/= "" key)
          do (setf (gethash (string-downcase key) table)
                   (read-header-val connection)))
    table))

(defun open-request (url &key headers method)
  (multiple-value-bind (schema host port path) (destructure-url url)
    (unless (string= schema "http")
      (error "URL schema ~s is not supported." schema))
    (let ((connection (open-connection host (or port 80))))
      (write-query-line connection (or method :get) path)
      (write-headers connection (list* (cons "Host" host) headers))
      (finish-output connection)
      (values (read-response-line connection)
              (read-headers connection)
              connection))))

(defun resolve-request (url &key headers method max-redirects)
  (loop for redirects from 0 below (or max-redirects 10)
        do (multiple-value-bind (code headers connection)
               (open-request url :headers headers :method method)
             (cond ((<= 300 code 399)
                    (setf url (gethash "location" headers))
                    (close-connection connection))
                   (T
                    (return (values code headers connection)))))
        finally (error "Exceeded ~a redirect~:p. Stopped at ~s"
                       max-redirects url)))

(defun call-with-request (function url &rest args)
  (multiple-value-bind (code headers connection) (apply #'resolve-request url args)
    (unwind-protect
         (funcall function code headers connection)
      (close-connection connection))))

(defmacro with-request ((code headers connection) (url &rest args) &body body)
  `(call-with-request (lambda (,code ,headers ,connection) ,@body) ,url ,@args))

(defun copy-stream-to-stream (in out &key buffer-size (element-type (stream-element-type in)) count)
  (let* ((buffer-size (or buffer-size 4096))
         (buffer (make-array buffer-size :element-type element-type)))
    (loop for left = (or count most-positive-fixnum) then (- left read)
          for read = (read-sequence buffer in :end (min buffer-size left))
          while (< 0 read)
          do (write-sequence buffer out :end read))))

(defun copy-bytestream-to-charstream (in out &key buffer-size count)
  (let* ((buffer-size (or buffer-size 4096))
         (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (loop for left = (or count most-positive-fixnum) then (- left read)
          for read = (read-sequence buffer in :end (min buffer-size left))
          while (< 0 read)
          do (loop for i from 0 below read
                   do (write-char (code-char (aref buffer i)) out)))))

(defun read-chunk-header (connection)
  (let* ((header (read-token connection -1))
         (end (or (position #\Return header)
                  (position #\; header))))
    (values (parse-integer header :end end :radix 16))))

(defun copy-chunked-body (connection stream &key buffer-size (copier #'copy-stream-to-stream))
  (loop for size = (read-chunk-header connection)
        while size
        do (funcall copier connection stream :buffer-size buffer-size :count size)
           (skip-until-crlf connection)))

(defun write-request-body (stream url &key headers method max-redirects buffer-size (copier #'copy-stream-to-stream))
  (with-request (code headers connection) (url :headers headers :method method :max-redirects max-redirects)
    (unless (= 200 code)
      (error "Unexpected response code ~s" code))
    (let ((content-length (ignore-errors (parse-integer (gethash "content-length" headers)))))
      (cond ((string= "chunked" (gethash "transfer-encoding" headers))
             (copy-chunked-body connection stream :buffer-size buffer-size :copier copier))
            (T
             (funcall copier connection stream :buffer-size buffer-size :count content-length))))))

(defun download-text (url &key headers method max-redirects buffer-size)
  (with-output-to-string (stream)
    (write-request-body stream url :headers (list* (cons "Connection" "close") headers)
                                   :method method
                                   :max-redirects max-redirects
                                   :buffer-size buffer-size
                                   :copier #'copy-bytestream-to-charstream)))

(defun download-file (file url &key headers method max-redirects buffer-size if-exists)
  (with-open-file (stream file :direction :output
                               :if-exists if-exists
                               :element-type '(unsigned-byte 8))
    (write-request-body stream url :headers (list* (cons "Connection" "close") headers)
                                   :method method
                                   :max-redirects max-redirects
                                   :buffer-size buffer-size)))

(defvar org.shirakumo.radiance.bootstrap::*template-start* ";;;; Radiance Launcher
;;; Please load this file in script mode.
;;; Some examples:
;;;   abcl --noinit --nosystem --batch --load start.lisp
;;;   ccl -n -b -l start.lisp
;;;   ecl -norc -shell start.lisp
;;;   sbcl --script start.lisp

;;; Sanity checks.
(unless *load-pathname*
  (error \"Please LOAD this file.\"))

(when (find-package :quicklisp)
  (error \"You must LOAD this file outside of your usual Quicklisp setup.\"))

;;; Find yourself.
(defpackage #:rad-bootstrap
  (:use #:cl)
  (:export #:*root* #:path))
(in-package #:rad-bootstrap)

(defvar *root* (make-pathname :name NIL :type NIL :defaults *load-pathname*))
(defun path (pathname)
  (merge-pathnames pathname *root*))

;;; Load Quicklisp and configure it.
(load (path \"quicklisp/setup.lisp\"))
(push (path \"modules/\") ql:*local-project-directories*)
(ql:register-local-projects)

;;; Load Radiance and configure it.
(ql:quickload '(#-sbcl prepl
                radiance))

(defmethod radiance:environment-directory (environment (kind (eql :configuration)))
  (rad-bootstrap:path (make-pathname :directory `(:relative \"config\" ,environment))))

(defmethod radiance:environment-directory (environment (kind (eql :cache)))
  (rad-bootstrap:path (make-pathname :directory `(:relative \"cache\" ,environment))))

(defmethod radiance:environment-directory (environment (kind (eql :data)))
  (rad-bootstrap:path (make-pathname :directory `(:relative \"data\" ,environment))))

(defmethod radiance:environment-directory (environment (kind (eql :template)))
  (rad-bootstrap:path (make-pathname :directory `(:relative \"override\" ,environment \"template\"))))

(defmethod radiance:environment-directory (environment (kind (eql :static)))
  (rad-bootstrap:path (make-pathname :directory `(:relative \"override\" ,environment \"static\"))))

(radiance:startup)

;;; Load all user modules and things.
(mapcar #'ql:quickload
        (radiance:find-all-modules (rad-bootstrap:path \"modules/\")))
(load (rad-bootstrap:path \"setup.lisp\"))

;;; Boot to REPL.
(sleep 1)
(in-package #:rad-user)
(unwind-protect
     (progn
       #-sbcl (prepl:repl)
       #+sbcl (sb-ext:enable-debugger)
       #+sbcl (sb-impl::toplevel-init))
  (when (radiance:started-p)
    (radiance:shutdown)))
")

(defvar org.shirakumo.radiance.bootstrap::*template-setup* ";;;; Radiance Setup
;;; Place configuration and setup forms in here.
;;;
;;; If you use the startup file to run Radiance, this file will
;;; be evaluated as well once Radiance has been started up.

(in-package #:rad-user)
")
(in-package #:org.shirakumo.radiance.bootstrap)

(defparameter *quickstart-url* "http://beta.quicklisp.org/quicklisp.lisp")
(defparameter *known-dists* '((shirakumo "http://dist.tymoon.eu/shirakumo.txt")
                              (quicklisp "http://beta.quicklisp.org/dist/quicklisp.txt")))
(defparameter *default-dists* "shirakumo quicklisp")
(defparameter *default-target* "~/radiance/")
(defparameter *default-hostnames* "example.com localhost")
(defparameter *default-port* "8080")
(defparameter *nonce* (gensym "NONCE"))

(defun split (string split)
  (let (pieces
        (buffer (make-string-output-stream)))
    (flet ((maybe-add-piece ()
             (let ((buffer (get-output-stream-string buffer)))
               (when (string/= "" buffer) (push buffer pieces)))))
      (loop for char across string
            do (if (char= char split)
                   (maybe-add-piece)
                   (write-char char buffer)))
      (maybe-add-piece)
      (nreverse pieces))))

(defun read-line* (&optional (stream *standard-input*) eof-error eof-value)
  (let ((read (read-line stream eof-error eof-value)))
    (if (string= "" read)
        eof-value
        read)))

(defun status (string &rest args)
  (format *query-io* "~& > ~?~%" string args)
  (finish-output *query-io*))

(defun complain (string &rest args)
  (format *query-io* "~& ! ~?~%" string args)
  (finish-output *query-io*))

(defun query (prompt &optional (default NIL d-p) validate)
  (format *query-io* "~& > ~a~%~@[ [~a]~] " prompt default)
  (finish-output *query-io*)
  (let ((read (read-line* *query-io* NIL *nonce*)))
    (if (eql read *nonce*) (setf read default))
    (multiple-value-bind (pass value) (if validate
                                          (funcall validate read)
                                          (values T read))
      (if pass
          value
          (query prompt default validate)))))

(defun dist-url (name)
  (second (assoc name *known-dists* :test #'string-equal)))

(defmacro f (package name &rest args)
  `(funcall (find-symbol ,(string name) ,(string package)) ,@args))

(defmacro s (package name)
  `(find-symbol ,(string name) ,(string package)))

(defun delete-empty-directory (directory)
  (rb-impl:feature-case
   (abcl
    (delete-file directory))
   ((or allegro clisp lispworks sbcl)
    (rb-impl:delete-directory directory))
   (ccl
    (rb-impl:delete-empty-directory directory))
   ((or cmucl scl)
    (rb-impl:unix-rmdir (namestring directory)))
   ((or clasp ecl mkcl)
    (rb-impl:rmdir directory))))

(defun delete-directory (directory)
  (when (probe-file directory)
    (dolist (file (directory (merge-pathnames "*.*" directory)))
      (if (or (pathname-name file)
              (pathname-type file))
          (delete-file file)
          (delete-directory file)))
    (delete-empty-directory directory)))

(defun load-quicklisp-quickstart ()
  (let ((ql-source (download-text *quickstart-url*)))
    (loop for start = 0 then end
          for (form end) = (multiple-value-list (read-from-string ql-source NIL *nonce* :start start))
          until (eql form *nonce*)
          do (unless (and (listp form)
                          (every #'string= form '(write-string *after-load-message*)))
               (eval form)))))

(defun install-quicklisp (target &key (dist-url (dist-url :quicklisp)))
  (load-quicklisp-quickstart)
  (f quicklisp-quickstart install :path target :dist-url dist-url))

(defun write-sexpr (out sexpr)
  (write sexpr :stream out :case :downcase))

(defun write-configuration (config-file hostnames port)
  (ensure-directories-exist config-file)
  (with-open-file (stream config-file :direction :output
                                      :element-type 'character)
    (format stream "; meta (:version 1.0 :package \"CL-USER\")~%")
    (write-sexpr stream
                 `((:interfaces
                    (:admin . "r-simple-admin")
                    (:auth . "r-simple-auth")
                    (:ban . "r-simple-ban")
                    (:cache . "r-simple-cache")
                    (:data-model . "r-simple-model")
                    (:database . "i-lambdalite")
                    (:logger . "i-verbose")
                    (:profile . "r-simple-profile")
                    (:rate . "r-simple-rate")
                    (:server . "i-hunchentoot")
                    (:session . "r-simple-sessions")
                    (:user . "r-simple-users"))
                   (:domains . ,hostnames)
                   (:port . ,port)
                   (:startup :r-simple-errors)
                   (:routes)))))

(defun write-startup (start)
  (with-open-file (stream start :direction :output
                                :element-type 'character)
    (write-string *template-start* stream)))

(defun write-setup (setup)
  (with-open-file (stream setup :direction :output
                                :element-type 'character)
    (write-string *template-setup* stream)))

(defun fixup-environment (target)
  (eval `(progn (defmethod ,(s radiance environment-directory) (environment (kind (eql :configuration)))
                  (merge-pathnames (make-pathname :directory `(:relative "config" ,environment))
                                   ,target))

                (defmethod ,(s radiance environment-directory) (environment (kind (eql :cache)))
                  (merge-pathnames (make-pathname :directory `(:relative "cache" ,environment))
                                   ,target))

                (defmethod ,(s radiance environment-directory) (environment (kind (eql :data)))
                  (merge-pathnames (make-pathname :directory `(:relative "data" ,environment))
                                   ,target))

                (defmethod ,(s radiance environment-directory) (environment (kind (eql :template)))
                  (merge-pathnames (make-pathname :directory `(:relative "override" ,environment "template"))
                                   ,target))

                (defmethod ,(s radiance environment-directory) (environment (kind (eql :static)))
                  (merge-pathnames (make-pathname :directory `(:relative "override" ,environment "static"))
                                   ,target)))))

(defun bootstrap (target dists hostnames port)
  (let* ((quicklisp (merge-pathnames "quicklisp/" target))
         (module (merge-pathnames "modules/" target))
         (config (merge-pathnames "config/" target))
         (config-file (merge-pathnames "default/radiance-core/radiance-core.conf.lisp" config))
         (setup (merge-pathnames "setup.lisp" target))
         (start (merge-pathnames "start.lisp" target)))
    (delete-directory target)
    (ensure-directories-exist module)
    (write-configuration config-file hostnames port)
    (write-setup setup)
    (write-startup start)
    (install-quicklisp quicklisp :dist-url (first dists))
    (setf *package* (find-package "CL-USER"))
    (dolist (dist (rest dists))
      (f ql-dist install-dist dist :prompt NIL))
    (f ql quickload '(prepl radiance))
    (fixup-environment target)
    (f radiance startup)
    (f radiance shutdown)
    (values module config config-file setup start)))

(defun to-directory-pathname (pathname)
  (if (or (pathname-name pathname)
          (pathname-type pathname))
      (make-pathname :name NIL
                     :type NIL
                     :directory (append (pathname-directory pathname)
                                        (list (format NIL "~@[~a~]~@[.~a~]"
                                                      (pathname-name pathname)
                                                      (pathname-type pathname))))
                     :defaults pathname)
      pathname))

(defun check-y-or-n (input)
  (values (find input '("yes" "y" "n" "no") :test #'string-equal)
          (find input '("yes" "y") :test #'string-equal)))

(defun check-install-target (pathname)
  (let ((pathname (to-directory-pathname pathname)))
    (if (probe-file pathname)
        (if (query "The directory already exists. It will be cleared out during installation. Is that Ok?"
                   "yes" #'check-y-or-n)
            (values T pathname))
        (values T pathname))))

(defun check-valid-dists (dists)
  (let ((dists (split dists #\Space)))
    (values (every #'dist-url dists)
            (mapcar #'dist-url dists))))

(defun check-valid-hostnames (hostnames)
  (values T
          (split hostnames #\Space)))

(defun check-port-number (number)
  (let ((integer (ignore-errors (parse-integer number))))
    (values integer integer)))

(defun install ()
  (let (target dists hostnames port)
    (status "Welcome to the Radiance bootstrapper.")
    (setf target (query "Where should Radiance be installed to?"
                        *default-target* #'check-install-target))
    (status "The following dists are known:~{~&  ~{~a  ~20t(~a)~}~}" *known-dists*)
    (setf dists (query "Which dists would you like to use?"
                       *default-dists* #'check-valid-dists))
    
    (setf hostnames (query "What hostnames is your machine reachable with?"
                           *default-hostnames* #'check-valid-hostnames))
    (setf port (query "Which port should Radiance run on?"
                      *default-port* #'check-port-number))
    (status "Configuration complete.")
    (status "Installing Radiance to ~a ..." target)
    (multiple-value-bind (module config config-file setup start)
        (bootstrap target dists hostnames port)
      (sleep 1)
      (status "Installation complete.~%")
      (status "Module directory:      ~a" module)
      (status "Environment directory: ~a" config)
      (status "Central configuration: ~a" config-file)
      (status "Custom setup file:     ~a" setup)
      (status "Radiance launcher:     ~a" start))))
(org.shirakumo.radiance.bootstrap:install)
