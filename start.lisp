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
(ql:quickload '(prepl radiance))
(setf radiance:*environment-root* (rad-bootstrap:path "config/"))
(radiance:startup)

;;; Load all user modules and things.
(mapcar #'ql:quickload
        (radiance:find-all-modules (rad-bootstrap:path "modules/")))
(load (rad-bootstrap:path "setup.lisp"))

;;; Boot to REPL.
(sleep 1)
(in-package #:rad-user)
(unwind-protect
     (prepl:repl)
  (when (radiance:started-p)
    (radiance:shutdown)))
