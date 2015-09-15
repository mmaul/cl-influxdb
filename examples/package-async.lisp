(defpackage :cl-influxdb.examples-async-v8
  (:use :cl
	:cl-influxdb-v8
	)
  (:import-from #:lparallel.promise
		:future
		:fulfilledp
		:fulfill
		:force
		:promise
		)
  (:import-from #:cl-csv
	       :get-data-table-from-csv
	       )
  (:import-from #:data-table
	       :data-table
	       :rows
	       )
  )


