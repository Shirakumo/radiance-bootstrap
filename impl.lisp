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
