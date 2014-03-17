CL-INFLUXDB
-----------

This package is a native Common Lisp interface for the InfluxDB time series database.

[InfluxDB](http://influxdb.org) is a scalable time series database.

Installation
------------
cl-influxdb will be distributed via [Quicklisp](http://quicklisp.ort).
It can also be obtained via it's [github repository] (http://github.com/mmaul/cl-influxdb)

Usage
-----
Below is the output of runnint the '''exercise''' form in examples/examples.lisp. This demonstrates
most of the functionality available in this library.

===========================================================================

Lets create a instance of class INFLUXDB to get started the default is
user = root, password = root, host = 127.0.0.1, port = 8086.
```
;;-----------------------------------CODE----------------------------------
(defparameter *db* "example")
(defparameter *app-user* "user")
(defparameter *app-password* "user")

(defparameter *influxdb*  (make-instance 'influxdb :database *db*))

(defparameter *user-db* (make-instance 'influxdb :database *db* 
				   :user *app-user*
				   :password *app-password*))
;;-------------------------------------------------------------------------

Results:
Group INFLUXDB
```
===========================================================================

First lets see if the server is alive
```
;;-----------------------------------CODE----------------------------------
(PING *INFLUXDB*)
;;-------------------------------------------------------------------------
Results:
((:STATUS . "ok"))
```
===========================================================================

Get list of defined databases and check to see if example database 
'example' exists.  If it does delete it.
```
;;-----------------------------------CODE----------------------------------
(LOOP FOR (NAME RESP) IN (GET-DATABASE-LIST *INFLUXDB*)
      DO (FORMAT T "~t ~a~%" (CDR NAME))
      WHEN (STRING= *DB* (CDR NAME))
        DO (PROGN (FORMAT T "Deleting: ~a~%" (CDR NAME))
                  (DELETE-DATABASE *INFLUXDB* (CDR NAME))))
;;-------------------------------------------------------------------------
  database
  example
Deleting: example

Results:
T
```
===========================================================================

Create a new database
```
;;-----------------------------------CODE----------------------------------
(HANDLER-CASE (CREATE-DATABASE *INFLUXDB* *DB*) (COMMAND-FAIL (E) E))
;;-------------------------------------------------------------------------
Results:
T
```
===========================================================================
 
If we had tried to create a database and it already existed
We get a condition, like this:
```
;;-----------------------------------CODE----------------------------------
(HANDLER-CASE (CREATE-DATABASE *INFLUXDB* *DB*)
              (COMMAND-FAIL (E) (PRINT E)))
;;-------------------------------------------------------------------------

Results:
#<COMMAND-FAIL #x3020048F05AD>

```
===========================================================================

Not that it's necessary for this simple test, but you might need
to create a seperate user for an application. lets add the user 'user'
```
;;-----------------------------------CODE----------------------------------
(ADD-DATABASE-USER *INFLUXDB* *APP-USER* *APP-PASSWORD*)
;;-------------------------------------------------------------------------

Results:
T
```
===========================================================================

Lets change the app user password
```
;;-----------------------------------CODE----------------------------------
(UPDATE-DATABASE-USER-PASSWORD *USER-DB* "newpass1")
;;-------------------------------------------------------------------------

Results:
T
```
===========================================================================
 
Load some data Note that all commands
```
;;-----------------------------------CODE----------------------------------
(WRITE-POINTS *USER-DB*
              '(((:NAME . RESPONSE_TIMES) (:COLUMNS TIME VALUE)
                 (:POINTS (1394761721 1.0) (1394761722 2.0))))
              :TIME-PRECISION
              'S)
;;-------------------------------------------------------------------------
Results:
T
```
===========================================================================

Now that we inserted some data lets list it.
```
;;-----------------------------------CODE----------------------------------
(QUERY *USER-DB* "select time from response_times;")
;;-------------------------------------------------------------------------
Results:
(((:NAME . "response_times") (:COLUMNS "time" "sequence_number" "value")
  (:POINTS (1394761 560001 2) (1394761 550001 1))))

```
===========================================================================

Lets insert some data dynamically for a data source on the web.
One thing to note is that when using symbols or keywords, is that they
 are down cased. So a series :MySeries will be created as 'myseries'
```
;;-----------------------------------CODE----------------------------------
(WRITE-POINTS *INFLUXDB*
              (LIST (LIST* '(:NAME . GASRATECO2)
                           (LIST* '(:COLUMNS INPUTGASRATE CO2)
                                  (LIST
                                   (CONS
                                    :POINTS
                                    (MAPCAR
                                     (LAMBDA
                                      (L)
                                      (MAPCAR
                                       #'ORG.MAPCAR.PARSE-NUMBER:PARSE-NUMBER
                                       (SPLIT-SEQUENCE:SPLIT-SEQUENCE
                                        #\,
                                        L)))
                                     (CDR
                                      (SPLIT-SEQUENCE:SPLIT-SEQUENCE
                                       #\Newline
                                       (DRAKMA:HTTP-REQUEST
                                        "http://datasets.connectmv.com/file/gas-furnace.csv")))))))))
              :TIME-PRECISION
              'S)
;;-------------------------------------------------------------------------

Results:
T
```
===========================================================================

Lets try a group by query...
```
;;-----------------------------------CODE----------------------------------
(QUERY *INFLUXDB*
       "select max(inputgasrate/co2) from gasrateco2 group by time(160m);")
;;-------------------------------------------------------------------------

Results:
(((:NAME . "gasrateco2") (:COLUMNS "time" "max")
  (:POINTS (1395024000 0.05624))))
```

===========================================================================

Lets clean up now.

Deleting database user 'user'
```
;;-----------------------------------CODE----------------------------------
(DELETE-DATABASE-USER *INFLUXDB* *APP-USER*)
;;-------------------------------------------------------------------------

Results:
T
===========================================================================
```

Deleting series response_times
```
;;-----------------------------------CODE----------------------------------
(DELETE-SERIES *INFLUXDB* :RESPONSE_TIMES)
;;-------------------------------------------------------------------------

Results:
T
```
===========================================================================

Deleting series gasrateco2
```
;;-----------------------------------CODE----------------------------------
(DELETE-SERIES *INFLUXDB* :GASRATECO2)
;;-------------------------------------------------------------------------

Results:
T
```
===========================================================================

Lets see what shards exist...
```
;;-----------------------------------CODE----------------------------------
(GET-SHARDS *INFLUXDB*)
;;-------------------------------------------------------------------------

Results:
((:LONG-TERM)
 (:SHORT-TERM
  ((:END-TIME . 1395273600) (:ID . 1) (:SERVER-IDS 1)
   (:START-TIME . 1394668800))
  ((:END-TIME . 1814400) (:ID . 2) (:SERVER-IDS 1)
   (:START-TIME . 1209600))))
Results:
T
```

TODO
====
Document and test Continous Queries
Async API

License
=======
Copyright (c) Mike Maul
Distributed under the MIT Licenses
