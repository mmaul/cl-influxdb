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
                             ))))

(asdf:defsystem :cl-influxdb.examples-async
  :description "Examples of using influxdb asynchyronously"
  :version "0.2.0"
  :author "Mike Maul <mike.maul@gmail.com>"
  :licence "MIT"
  :encoding :utf-8
  :depends-on (#:cl-influxdb 
               #:cl-annot
               #:parse-number 
               #:split-sequence 
               #:lparallel
               #:cl-csv-data-table
               #:data-table
               )
  :components ((:module "examples"
                :serial t
                :components ((:file "package-async")
                             (:file "examples-async")
                             ))))

(asdf:defsystem :cl-influxdb.doc
  :description "Create documentation for cl-influxdb"
  :version "0.2.0"
  :author "Mike Maul <mike.maul@gmail.com>"
  :licence "MIT"
  :encoding :utf-8
  :depends-on (#:cl-influxdb
               #:cl-influxdb.examples
               #:cl-influxdb.examples-async
               #:cl-annot)
  :components ((:file "doc")
               ))


#-sbcl
(asdf:defsystem :cl-influxdb.tests
  :description "Tests for cl-influxdb library"
  :version "0.2.0"
  :author "Mike Maul <mike.maul@gmail.com>"
  :licence "MIT"
  :encoding :utf-8
  :depends-on (#:cl-influxdb
               #:nst)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests")
                             ))))
#-sbcl
(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :cl-influxdb))))
  (asdf:load-system :cl-influxdb.tests)
  (funcall (find-symbol (symbol-name :run-tests) :cl-influxdb.tests)))

