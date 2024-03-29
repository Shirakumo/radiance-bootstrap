(in-package #:org.shirakumo.radiance.bootstrap)

(defparameter *quickstart-url* "http://beta.quicklisp.org/quicklisp.lisp")
(defparameter *known-dists* '((shirakumo "http://dist.shirakumo.org/shirakumo.txt")
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
    (f ql quickload '(radiance))
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
