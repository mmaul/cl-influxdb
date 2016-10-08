(in-package cl-influxdb.examples)
(annot:enable-annot-syntax)

(defparameter db "cl_influxdb_example")
(defparameter influxdb nil)
(defparameter user "root")
(defparameter password "root")

;; Preform some operations to exercise the interface
@export
(defun exercise ()

  
    (print-run "
Usage
=====

Lets create a instance of class INFLUXDB to get started the default is
user = root, password = root, host = 127.0.0.1, port = 8086.
"
    (setq influxdb  (make-instance 'influxdb :database db :user user :password password))               
               )


        
    (print-run "
Create a new database"
               
               (handler-case (create-database influxdb db)
                 (command-fail (e) e)))
    
    (print-run " 
If we had tried to create a database and it already existed
We get a condition, like this:"
               
               (handler-case (create-database influxdb db)
                 (command-fail (e) (print  e)))
               
               )
    
    (print-run " 
Load some data Note that all commands"
                   
                   (write-points influxdb 
                                 (make-influxdb-data :key :response_times :tags '((:host . "server1") (:region . "one")) :columns '(:t1 :v1) :points '((1394761721 1.0) 
                                     (1394761722 2.0))))
                   
                   )
        
        (print-run "
Now that we inserted some data lets list it.
"
                   
                   (query influxdb "select t1,p1 from response_times;")
                   
                   )
        

        t)
