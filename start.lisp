;;;; Radiance Launcher
;;; Please load this file in script mode.
;;; Some examples:
;;;   abcl --noinit --nosystem --batch --load start.lisp
;;;   ccl -n -b -l start.lisp
;;;   ecl -norc -shell start.lisp
;;;   sbcl --script start.lisp

;;; Sanity checks.
(unless *load-pathname*
  (error "Please LOAD this file."))

(when (find-package :quicklisp)
  (error "You must LOAD this file outside of your usual Quicklisp setup."))

;;; Find yourself.
(defpackage #:rad-bootstrap
  (:use #:cl)
  (:export #:*root* #:path))
(in-package #:rad-bootstrap)

(defvar *root* (make-pathname :name NIL :type NIL :defaults *load-pathname*))
(defun path (pathname)
  (merge-pathnames pathname *root*))

;;; Load Quicklisp and configure it.
(load (path "quicklisp/setup.lisp"))
(push (path "modules/") ql:*local-project-directories*)
(ql:register-local-projects)

;;; Load Radiance and configure it.
(ql:quickload '(#-sbcl prepl
                radiance))

(defmethod radiance:environment-directory (environment (kind (eql :configuration)))
  (rad-bootstrap:path (make-pathname :directory `(:relative "config" ,environment))))

(defmethod radiance:environment-directory (environment (kind (eql :cache)))
  (rad-bootstrap:path (make-pathname :directory `(:relative "cache" ,environment))))

(defmethod radiance:environment-directory (environment (kind (eql :data)))
  (rad-bootstrap:path (make-pathname :directory `(:relative "data" ,environment))))

(defmethod radiance:environment-directory (environment (kind (eql :template)))
  (rad-bootstrap:path (make-pathname :directory `(:relative "override" ,environment "template"))))

(defmethod radiance:environment-directory (environment (kind (eql :static)))
  (rad-bootstrap:path (make-pathname :directory `(:relative "override" ,environment "static"))))

(radiance:startup)

;;; Load all user modules and things.
(mapcar #'ql:quickload
        (radiance:find-all-modules (rad-bootstrap:path "modules/")))
(load (rad-bootstrap:path "setup.lisp"))

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
