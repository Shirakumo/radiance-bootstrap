(asdf:defsystem radiance-bootstrap
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A bootstrapper for Radiance installations"
  :homepage "https://github.com/Shirakumo/radiance-bootstrap"
  :serial T
  :build-operation asdf:monolithic-concatenate-source-op
  :build-pathname "radiance-bootstrap"
  :components ((:file "impl")
               (:file "net")
               (:file "url")
               (:file "http")
               (:file "boot")))
