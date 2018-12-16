(defpackage #:radiance-bootstrap-asdf
  (:use #:cl))
(in-package #:radiance-bootstrap-asdf)

(defmethod concatenate-source ((file asdf:cl-source-file) output)
  (with-open-file (input (asdf:component-pathname file) :direction :input)
    (uiop:copy-stream-to-stream input output)))

(defclass template (asdf:cl-source-file)
  ())

(defmethod concatenate-source ((file template) output)
  (format output "~&~%(defvar org.shirakumo.radiance.bootstrap::*template-~a* \""
          (pathname-name (asdf:component-pathname file)))
  (unwind-protect
       (with-open-file (input (asdf:component-pathname file) :direction :input)
         (loop for c = (read-char input NIL)
               while c
               do (when (or (eql c #\\) (eql c #\"))
                    (write-char #\\ output))
                  (write-char c output)))
    (format output "\")~%")))

(defmethod asdf:perform ((o asdf:compile-op) (c template))
  (let ((*print-case* (readtable-case *readtable*)))
    (proclaim `(special ,(intern (format NIL "*~a-~a*"
                                         '#:template (pathname-name (asdf:component-pathname c))))))))

(defmethod asdf:perform ((o asdf:load-op) (c template))
  (setf *package* (find-package '#:org.shirakumo.radiance.bootstrap))
  (eval (read-from-string (with-output-to-string (out)
                            (concatenate-source c out)))))

(defclass bootstrap-concat-op (asdf:monolithic-concatenate-source-op)
  ())

(defun input-components (s)
  (loop for c in (asdf:required-components
                  s :goal-operation 'asdf:compile-op
                    :keep-operation 'asdf:compile-op
                    :other-systems T)
        when (typep c 'asdf:cl-source-file)
        collect c))

(defmethod asdf:perform ((o bootstrap-concat-op) (s asdf:system))
  (let* ((ins (input-components s))
         (out (asdf:output-file o s))
         (tmp (uiop:tmpize-pathname out)))
    (with-open-file (output tmp :direction :output :if-exists :rename-and-delete)
      (dolist (input ins)
        (concatenate-source input output))
      (format output "(org.shirakumo.radiance.bootstrap:install)"))
    (uiop:rename-file-overwriting-target tmp out)))

(asdf:defsystem radiance-bootstrap
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A bootstrapper for Radiance installations"
  :homepage "https://Shirakumo.github.io/radiance-bootstrap/"
  :bug-tracker "https://github.com/Shirakumo/radiance-bootstrap/issues"
  :source-control (:git "https://github.com/Shirakumo/radiance-bootstrap.git")
  :serial T
  :build-operation bootstrap-concat-op
  :build-pathname "bin/radiance-bootstrap"
  :components ((:file "package")
               (:file "impl")
               (:file "net")
               (:file "url")
               (:file "http")
               (template "start")
               (template "setup")
               (:file "boot")))
