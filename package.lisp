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
