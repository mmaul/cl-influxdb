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
               (:file "cl-influxdb")))

(asdf:defsystem :cl-influxdb.examples
  :description "Examples for influxdb"
  :version "0.2.0"
  :author "Mike Maul <mike.maul@gmail.com>"
  :licence "MIT"
  :encoding :utf-8
  :depends-on ("cl-influxdb" #:parse-number #:split-sequence)
  :components ((:module "examples"
			:serial t
			:components ((:file "package")
				     (:file "examples")
				     ))))

(asdf:defsystem :cl-influxdb.tests
  :description "Tests for cl-influxdb library"
  :version "0.2.0"
  :author "Mike Maul <mike.maul@gmail.com>"
  :licence "MIT"
  :encoding :utf-8
  :depends-on ("cl-influxdb" "nst")
  :components ((:module "tests"
			:serial t
			:components ((:file "package")
				     (:file "tests")
				     ))))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :cl-influxdb))))
  (asdf:load-system :cl-influxdb.tests)
  (funcall (find-symbol (symbol-name :run-tests) :cl-influxdb.tests)))

