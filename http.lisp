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
