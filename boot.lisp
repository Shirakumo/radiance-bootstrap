(defpackage #:org.shirakumo.radiance.bootstrap
  (:nicknames #:rb)
  (:use #:cl #:org.shirakumo.radiance.bootstrap.http)
  (:export #:bootstrap
           #:install))
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
  `(symbol-value (find-symbol ,(string name) ,(string package))))

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

(defun write-configuration (config-dir hostnames port)
  (let ((core (merge-pathnames "default/radiance-core/radiance-core.conf.lisp" config-dir)))
    (ensure-directories-exist core)
    (with-open-file (stream core :direction :output
                                 :element-type 'character)
      (write-sexpr stream
                   `((:interfaces
                      (:logger . "i-verbose")
                      (:data-model . "r-simple-model")
                      (:database . "i-lambdalite")
                      (:user . "r-simple-users")
                      (:auth . "r-simple-auth")
                      (:session . "r-simple-sessions")
                      (:server . "i-hunchentoot"))
                     (:server
                      (:domains . ,hostnames)
                      (:instances . (((:port . ,port)))))
                     (:startup :r-simple-errors))))))

(defun write-startup (start quicklisp-dir module-dir config-dir)
  (with-open-file (stream start :direction :output
                                :element-type 'character)
    (format stream ";;;; Radiance Launcher
;;; Please load this file in script mode.
;;; Some examples:
;;;   abcl --noinit --nosystem --batch --load ~2:*~a~@[.~a~]
;;;   ccl -n -b -l ~2:*~a~@[.~a~]
;;;   ecl -norc -shell ~2:*~a~@[.~a~]
;;;   sbcl --script ~a~@[.~a~]

\(load #p~s)
\(push #p~s ql:*local-project-directories*)
\(ql:register-local-projects)
\(ql:quickload '(prepl radiance))
\(in-package #:rad-user)

\(setf radiance:*environment-root* #p~s)
\(radiance:startup)
\(sleep 0.1)
\(unwind-protect
    (prepl:repl)
  (radiance:shutdown))"
            (pathname-name start) (pathname-type start)
            (merge-pathnames "setup.lisp" quicklisp-dir)
            module-dir config-dir)))

(defun bootstrap (target dists hostnames port)
  (let ((quicklisp (merge-pathnames "quicklisp/" target))
        (module (merge-pathnames "modules/" target))
        (config (merge-pathnames "config/" target))
        (start (merge-pathnames "start.lisp" target)))
    (delete-directory target)
    (ensure-directories-exist module)
    (write-configuration config hostnames port)
    (write-startup start quicklisp module config)
    (install-quicklisp quicklisp :dist-url (first dists))
    (dolist (dist (rest dists))
      (f ql-dist install-dist dist :prompt NIL))
    (f ql quickload '(prepl radiance))
    (setf (s radiance *environment-root*) config)
    (f radiance startup)
    (f radiance shutdown)
    (sleep 1)))

(defun to-directory-pathname (pathname)
  (if (or (pathname-name pathname)
          (pathname-type pathname))
      (merge-pathnames (format NIL "~@[~a~]~@[.~a~]"
                               (pathname-name pathname)
                               (pathname-type pathname)))
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
    (bootstrap target dists hostnames port)
    (status "Installation complete.~%")
    (status "Install custom modules to: ~a" (merge-pathnames "modules/" target))
    (status "In order to run Radiance:  ~a" (merge-pathnames "start.lisp" target))))

(install)
