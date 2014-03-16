(in-package cl-influxdb.examples)


(defparameter *db* "example")
(defparameter *app-user* "user")

(defparameter *app-password* "user")
(defparameter *influxdb*  (make-instance 'influxdb :database *db*))

(defparameter *user-db* (make-instance 'influxdb :database *db* 
				   :user *app-user*
				   :password *app-password*)
  
  )


;; Preform some operations to exercise the interface
(defun exercise ()

  (setq *db* "example")
  (setq *app-user* "user")
  (setq *app-password* "user")
  (print-run "
Lets create a instance of class INFLUXDB to get started the default is
user = root, password = root, host = 127.0.0.1, port = 8086.
"
	     (setq *influxdb*  (make-instance 'influxdb :database *db*)))

  (print-run "
Creating another instance if INFLUX DB to test database user commands later.
"
	     (setq *user-db* (make-instance 'influxdb :database *db* 
					    :user *app-user*
					    :password *app-password*))
	     
	     )

  (print-run "
First lets see if the server is alive
"
	     (ping *influxdb*)
	     
	     )
  
  (print-run "
Get list of defined databases and check to see if example database 
'example' exists.  If it does delete it."
   
	     (loop for (name resp) in (get-database-list *influxdb*)
		do (format t "~t ~a~%" (cdr name))
		when (string= *db* (cdr name)) do 
		  (progn 
		    (format t "Deleting: ~a~%" (cdr name))
		    (delete-database *influxdb* (cdr name))
		    ))
	     
	     )

  (print-run "
Create a new database"

	     (handler-case (create-database *influxdb* *db*)
	       (command-fail (e) e)))

  (print-run " 
If we had tried to create a database and it already existed
We get a condition, like this:"
	     
	     (handler-case (create-database *influxdb* *db*)
	       (command-fail (e) (print  e)))
	     
	     )

  (print-run "
Not that it's necessary for this simple test, but you might need
to create a seperate user for an application. lets add the user 'user'"
	     
	     (add-database-user *influxdb* *app-user* *app-password*)
	     
	     )

  ;; Lets change to the application user
  
    (print-run "
Lets change the app user password"
	     
	       (update-database-user-password *user-db* "newpass1")
	       
	       )
    (print-run " 
Load some data Note that all commands"
	       
	       (write-points *user-db* 
		    '(  ;; Write just a few data points 
		      ((:NAME . response_times) 
		       (:COLUMNS time value )
		       (:POINTS 
			(1394761721 1.0) 
			(1394761722 2.0)) 
		       )) :time-precision 's)
	       
	       )
    
    (print-run "
Now that we inserted some data lets list it.
"
      
	       (query *user-db* "select time from response_times;")
	       
	       )
    
    (print-run "
Lets insert some data dynamically for a data source on the web.
One thing to note is that when using symbols or keywords, is that they
 are down cased. So a series :MySeries will be created as 'myseries'
"
	       (write-points *influxdb* ;; Write dynamic data from the net 
		 `(((:NAME . GasRateCO2) 
		    (:COLUMNS InputGasRate CO2 )
		    ,(cons :POINTS 
			   (mapcar (lambda (l) 
				     (mapcar #'parse-number:parse-number  
					   (split-sequence:split-sequence #\, l)))
				   (cdr (split-sequence:split-sequence 
					 #\newline 
			    (drakma:http-request
			     "http://datasets.connectmv.com/file/gas-furnace.csv")
			    )))))) 
		 :time-precision 's)
	       
	       )

    (print-run "
Lets try a group by query..."
	        (query *influxdb* "select max(inputgasrate/co2) from gasrateco2 group by time(160m);")
		
		)
    
    (print-run "
Lets clean up now.

Deleting database user 'user'
"
	       (delete-database-user *influxdb* *app-user*)

	       )
    
    (print-run "
Deleting series response_times
"
	       (delete-series *influxdb* :response_times)

	       )
    
    (print-run "
Deleting series gasrateco2
"
	       (delete-series *influxdb* :gasrateco2)

	       )
    (print-run "
Lets see what shards exist...
"
	       (get-shards *influxdb*)

	       
	       )
    t    
    )
