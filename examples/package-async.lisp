(defpackage :cl-influxdb.examples-async
  (:use :cl
	:cl-influxdb
	)
  (:import-from #:lparallel.promise
		:future
		:fulfilledp
		:fulfill
		:force
		:promise
		)
  )

(in-package :cl-influxdb.examples-async)
