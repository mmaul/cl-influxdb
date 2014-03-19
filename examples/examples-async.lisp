(in-package cl-influxdb.examples-async)
(annot:enable-annot-syntax)

(defparameter *db* "example-async")

(defparameter *influxdb*  (make-instance 'influxdb :database *db*))



;; Preform some operations to exercise the interface
@export
(defun exercise ()
  (print-run "
Asyncronous Usage
=================

Rather that create an async query mechanism or write wrappers we will
just just lparallel ought right. It is simpler and more elegant.

First we just need to create a lparallel kernel which maintains a
thread pool. The integer parameter to make-kernel specifies the number
of CPU cores available to lparallel
"
	   (when (not lparallel:*kernel*)
	     (setf lparallel:*kernel* (lparallel:make-kernel 2))))




  (print-run "
Create a new database 'example-async unless it already exists"

	     (handler-case (create-database *influxdb* *db*)
	       (command-fail (e) (print  e))))

  (print-run " 
Repeating the bulk load from prefious examples, we use lparallel futures to preform the operation asynchronously.

The pattern is: 
* Make a promise
* Make future form that fuflills promise
* Wait and/or do something else 
* Check for the fulfillment to be complete or call force which blocks

For example something that waits 10 seconds for a future to complete then forces it.
```
  (let ((p (promise))) 
    (future (progn (sleep .3) (fulfill p 'done)))
    (format t \"Wait for future to come\") 
    (loop for i from 1 to 10 
      when (not (fulfilledp p)) do 
        (progn (print i) (sleep .1)))
    (force p))
```
Get it? On to using this pattern with write-points
"
	       
	       (let ((p (promise)))
		 (future 
		   (let ((result 
			  (write-points *influxdb* ;; Write dynamic data from the net 
			    `(((:NAME . GasRateCO2) 
			       (:COLUMNS InputGasRate CO2 )
			       ,(cons :POINTS 
				  (data-table:rows 
				   (cl-csv:get-data-table-from-csv 
				    (drakma:http-request
				     "http://datasets.connectmv.com/file/gas-furnace.csv" :want-stream t)))
				  
				      ))) 
			    :time-precision 's)))
				(format t "Heyyyyy I'm done!!!!!~%")
				(fulfill p result)
				)
			      )
		 (format t "Right, lets sleep for a second...~%")
		 (sleep 1)
		 (if (lparallel.promise:fulfilledp p) 
		     (format t "Looks like it's done~%")
		     (format t "Looks like it's not done yet. Well we will force it, then~%"))
		 (print (force p)))
	       
	       )

    
    (print-run "
Lets try a group by query...asynchronously
We will request fulfillment wait up to 10 seconds and the get the results
"
	       (let ((p (promise)))
		 (future (fulfill p (query *influxdb*
					   "select max(inputgasrate/co2) from gasrateco2 group by time(160m);")))
		 (loop for i from 1 to 1000
		       when (not (fulfilledp p)) do
		      (progn  (format t "Wait ~fms~%" (/ i 100)) (sleep .01)))
		 
		 (force p)
		 )
	       
	       )

  (print-run "
Remove the new database 'example-async"

	     (handler-case (delete-database *influxdb* *db*)
	       (command-fail (e) (print  e))))

    t    
    )
