(defpackage #:org.shirakumo.radiance.bootstrap
  (:nicknames #:rb)
  (:use #:cl #:org.shirakumo.radiance.bootstrap.http))
(in-package #:org.shirakumo.radiance.bootstrap)

(defparameter *quickstart-url* "http://beta.quicklisp.org/quicklisp.lisp")
(defparameter *known-dists* '((shirakumo "http://dist.tymoon.eu/shirakumo.txt")
                              (quicklisp "http://beta.quicklisp.org/dist/quicklisp.txt")))
(defparameter *default-dists* '(shirakumo quicklisp))
(defparameter *default-target* #p"~/radiance/")
(defparameter *default-hostnames* "example.com localhost")
(defparameter *default-port* 8080)
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

(defun read-pathname (&optional (stream *standard-input*) eof-error eof-value)
  (let ((read (read-line stream eof-error eof-value)))
    (if (string= "" read)
        eof-value
        (pathname read))))

(defun read-list (&optional (stream *standard-input*) eof-error eof-value)
  (let ((read (read-line stream eof-error eof-value)))
    (split read #\Space)))

(defun read-integer (&optional (stream *standard-input*) eof-error eof-value)
  (let ((read (read stream eof-error eof-value)))
    (if (integerp read)
        read
        eof-value)))

(defun dist-url (name)
  (second (assoc name *known-dists* :test #'string-equal)))

(defmacro f (package name &rest args)
  `(funcall (find-symbol ,(string name) ,(string package)) ,@args))

(defun status (string &rest args)
  (format *query-io* "~& > ~?~%" string args))

(defun complain (string &rest args)
  (format *query-io* "~& ! ~?~%" string args))

(defun query (prompt &optional (default NIL d-p) (reader #'read))
  (format *query-io* "~&~a~@[ [~a~] " prompt default)
  (let ((read (funcall reader *query-io* NIL *nonce*)))
    (cond ((not (eql read *nonce*))
           read)
          (d-p
           default)
          (T
           (complain "I need an answer.")
           (query prompt default reader)))))

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
  (quicklisp-quickstart:install :path target :dist-url dist-url))

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

(defun write-startup (start quicklisp-dir config-dir)
  (with-open-file (stream start :direction :output
                                :element-type 'character)
    (format stream ";;;; Radiance startup file
;;; Please load this file in script mode. For SBCL, this would be:
;;;   sbcl --script ~a.~a
;;;

\(load #p~s)
\(ql:quickload :radiance)
\(in-package #:rad-user)

\(setf radiance:*environment-root* #p~s)
\(radiance:startup)"
            (pathname-name start) (pathname-type start)
            (merge-pathnames "setup.lisp" quicklisp-dir)
            config-dir)))

(defun bootstrap (target dists hostnames port)
  (let ((quicklisp (merge-pathnames "quicklisp/" target))
        (config (merge-pathnames "config/" target))
        (start (merge-pathnames "start.lisp" target)))
    (install-quicklisp quicklisp :dist-url (first dists))
    (dolist (dist dists)
      (f ql-dist install-dist (dist-url dist)))
    (write-configuration config hostnames port)
    (write-startup start quicklisp config)))

(defun main ()
  (let (target dists hostnames port)
    (setf target (query "Where should Radiance be installed to?" *default-target* #'read-pathname))
    (status "The following dists are known:~{~&  ~{~a  ~20t(~a)~}~}" *known-dists*)
    (loop do (setf dists (query "Which dists would you like to use?" *default-dists* #'read-list))
             (if (every #'dist-url dists)
                 (return)
                 (complain "Please enter a valid list of dist names.")))
    (setf hostnames (query "What hostnames is your machine reachable with?" *default-hostnames* #'read-list))
    (setf port (query "Which port should Radiance run on?" *default-port* #'read-integer))
    (status "Configuration complete.")
    (status "Installing Radiance to ~s..." target)
    (bootstrap target dists hostnames port)))
