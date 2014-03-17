(in-package :cl-influxdb.tests)

(nst:def-fixtures f-cl-influxdb
  ()
  (*db* "test")
  (*app-user* "user")
  (*app-password* "user")
  (*influxdb*  (make-instance 'influxdb :database *db*))
  (*user-db* (make-instance 'influxdb :database *db* 
			    :user *app-user*
			    :password *app-password*)
    
    ))


(nst:def-test-group group1 (f-cl-influxdb)
  (:cleanup (handler-case (delete-database *influxdb* *db*) (error () nil)))
  
  (nst:def-test ping-test
      (:equal "ok") 
    (cdr (assoc :STATUS (ping *influxdb*)))
    )
  
  (nst:def-test 
      (create-database-test  :setup (handler-case (delete-database *influxdb* *db*) (error () nil)))
      (:true) (nth-value 0 (create-database *influxdb* *db*))
      )
  
  (nst:def-test get-database-list-test
      (:true) (loop for (name resp) in (get-database-list *influxdb*) 
	 when (string= *db* (cdr name)) do (return t))
      )
  
  (nst:def-test (add-database-user :cleanup (handler-case (delete-database-user *influxdb* *app-user*)))
      (:true ) (nth-value 0 
			  (add-database-user *influxdb* 
					     *app-user* *app-password*)))
  
  (nst:def-test (update-database-user-password 
	       :startup   (handler-case (add-database-user *influxdb* *app-user* *app-password*))
	       :cleanup (handler-case (delete-database-user *influxdb* *app-user*)))
      (:true ) (nth-value 0 
			  (update-database-user-password *influxdb* 
					     *app-user* *app-password*)))

  (nst:def-test write-points-test 
      (:true) (nth-value 0 
			 (write-points *influxdb* 
				       '(  ;; Write just a few data points 
					 ((:NAME . response_times) 
					  (:COLUMNS time value )
					  (:POINTS 
					   (1394761721 1.0) 
					   (1394761722 2.0)) 
					  )) :time-precision 's)))
  
  (nst:def-test query-test
      (:equal "response_times") 
    (cdr (assoc :name (car 
		       (query *influxdb* "select time from response_times;"))))
    )
  )


  
