(defpackage #:org.shirakumo.radiance.bootstrap.url
  (:nicknames #:rb-url)
  (:use #:cl)
  (:export #:destructure-url))
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
