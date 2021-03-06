;;;; cl-influxdb.asd

(asdf:defsystem #:cl-influxdb
  :serial t
  :description "Common lisp binding for InfluxDB"
  :author "Mike Maul <mike.maul@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-json
               #:drakma
               #:do-urlencode
               #:cl-annot
               #:flexi-streams
               #:usocket
	       )
  :components ((:file "package")
               (:file "cl-influxdb")
               (:file "cl-influxdb-v8")))

(asdf:defsystem :cl-influxdb.examples
  :description "Examples for influxdb"
  :version "0.2.0"
  :author "Mike Maul <mike.maul@gmail.com>"
  :licence "MIT"
  :encoding :utf-8
  :depends-on (#:cl-influxdb 
               #:cl-annot
               #:parse-number 
               #:split-sequence)
  :components ((:module "examples"
                :serial t
                :components ((:file "package")
                             (:file "examples")
                             (:file "examples-v8")
                             ))))

