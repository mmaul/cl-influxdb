
CL-INFLUXDB
-------------

This package is a native Common Lisp interface for the InfluxDB time series database.

[InfluxDB](http://influxdb.org) is a scalable time series database.

News
----
Due to major changes in InfluxDB in starting with version 0.9 seperate packages have been created to work with the versions less than 0.9 and versions 0.9 and greater. The interface `cl-influxdb` that works with version 0.8 and below has been changed to `cl-influxdb-v8` specific information regarding the v8 interface can be obtained by reading README-v8.md.The interface that works with 0.9 is now `cl-influxdb`.
    
One of the major changes in version 0.9 and above is that much functionality including most administrative functions have been pulled into the InfluxDB query language. Therefor many of the administrative interface functions have been removed from this interface.

Installation
------------
cl-influxdb is distributed via [Quicklisp](http://quicklisp.ort).
It can also be obtained via it's [github repository] (http://github.com/mmaul/cl-influxdb)

Usage
------
For query and administrative commands see the [InfluxDB Documentation](https://influxdb.com/docs/v0.9/). All administrative commands are issued via the `query` method.

Load library and create a instance of class INFLUXDB, using the default is
user = root, password = root, host = 127.0.0.1, port = 8086.



    (ql:quickload :cl-influxdb)       

    To load "cl-influxdb":
      Load 1 ASDF system:
        cl-influxdb
    
    ; Loading "cl-influxdb"
    ..........





    (:CL-INFLUXDB)




    (defpackage #:cl-influxdb-example
      (:use #:cl #:cl-influxdb
            ))




    #<PACKAGE "CL-INFLUXDB-EXAMPLE">




    (in-package :cl-influxdb-example)




    #<PACKAGE "CL-INFLUXDB-EXAMPLE">



Lets create a instance of class INFLUXDB to get started the default is
user = root, password = root, host = 127.0.0.1, port = 8086.


    (defparameter influxdb  (make-instance 'influxdb :database "cl_influxdb_example" :user "root" :password "root"))    




    INFLUXDB




    (ping influxdb)




    T



Create a database, or if the database already exists handle the `command-fail` condition.


    (handler-case (create-database influxdb "cl_influxdb_example" ) (command-fail (e) e))




    T



Data is writtent by populating a influxdb-data structure and writing it to the database


    (write-points influxdb 
        (make-influxdb-data :key :response_times :tags '((:host . "server1") (:region . "one")) 
            :columns '(:t1 :v1 :c1) 
            :points '((1394761721 "Y" 1.0 1442377210) 
                      (1394761722 "X" 2.0 1442377211)))
                      :time-precision :s)




    T




    (query influxdb "select time,v1,c1,t1 from response_times;")




    ((:RESULTS
      ((:SERIES
        ((:NAME . "response_times") (:COLUMNS "time" "v1" "c1" "t1")
         (:VALUES ("2015-09-16T04:20:10Z" "Y" 1 1394761721)
          ("2015-09-16T04:20:11Z" "X" 2 1394761722)))))))



Now clean up and drop the database


    (query influxdb "drop database cl_influxdb_example")




    ((:RESULTS NIL))



License
====
The MIT License (MIT)

Copyright (c) 2014 Michael Maul

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

