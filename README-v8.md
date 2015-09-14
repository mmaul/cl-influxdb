CL-INFLUXDB
-----------

This package is a native Common Lisp interface for the InfluxDB time series database.

[InfluxDB](http://influxdb.org) is a scalable time series database.

Installation
------------
cl-influxdb will be distributed via [Quicklisp](http://quicklisp.ort).
It can also be obtained via it's [github repository] (http://github.com/mmaul/cl-influxdb)

Under SBCL the system cl-influxdb.tests is excluded to to incompatibilities with NST and 
SBCL's enforcement of *pprint-dispatch* immutability.


===========================================================================

Usage
=====

Lets create a instance of class INFLUXDB to get started the default is
user = root, password = root, host = 127.0.0.1, port = 8086.

```
;;-----------------------------------CODE----------------------------------
(SETQ *INFLUXDB* (MAKE-INSTANCE 'INFLUXDB :DATABASE *DB*))
;;-------------------------------------------------------------------------

Results:

#<INFLUXDB #x302003474EFD>
```
===========================================================================

Creating another instance if INFLUX DB to test database user commands later.

```
;;-----------------------------------CODE----------------------------------
(SETQ *USER-DB*
      (MAKE-INSTANCE
        'INFLUXDB
        :DATABASE
        *DB*
        :USER
        *APP-USER*
        :PASSWORD
        *APP-PASSWORD*))
;;-------------------------------------------------------------------------

Results:

#<INFLUXDB #x3020034744DD>
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
  example
Deleting: example

Results:

NIL
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

#<COMMAND-FAIL #x3020034AC29D> 
Results:

#<COMMAND-FAIL #x3020034AC29D>
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

(((:NAME . "response_times")
  (:COLUMNS "time" "sequence_number" "value")
  (:POINTS (1394761 260001 2) (1394761 250001 1))))
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
  (:POINTS (1395196800 0.05624))))
```
===========================================================================

Now for continous queries, generally these are used for precomputed rollups.
Lets list the current precomputed queries.

```
;;-----------------------------------CODE----------------------------------
(LIST-CONTINUOUS-QUERIES *INFLUXDB*)
;;-------------------------------------------------------------------------

Results:

(((:ID . 1)
  (:QUERY
   . "select mean(inputgasrate), mean(co2) from gasrateco2 group by time(1h) into gasrateco2.1h;"))
)
```
===========================================================================

Now to create a continous query, 


```
;;-----------------------------------CODE----------------------------------
(CREATE-CONTINUOUS-QUERIES
  *INFLUXDB*
  "select mean(inputgasrate), mean(co2) from gasrateco2 group by time(1h) into gasrateco2.1h;")
;;-------------------------------------------------------------------------

Results:

T
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
```
===========================================================================

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
```
===========================================================================

Asyncronous Usage
=================

Rather that create an async query mechanism or write wrappers we will
just just lparallel ought right. It is simpler and more elegant.

First we just need to create a lparallel kernel which maintains a
thread pool. The integer parameter to make-kernel specifies the number
of CPU cores available to lparallel

```
;;-----------------------------------CODE----------------------------------
(WHEN (NOT LPARALLEL.KERNEL:*KERNEL*)
  (SETF LPARALLEL.KERNEL:*KERNEL* (LPARALLEL.KERNEL:MAKE-KERNEL 2)))
;;-------------------------------------------------------------------------

Results:

NIL
```
===========================================================================

Create a new database 'example-async unless it already exists
```
;;-----------------------------------CODE----------------------------------
(HANDLER-CASE (CREATE-DATABASE
                CL-INFLUXDB.EXAMPLES-ASYNC::*INFLUXDB*
                CL-INFLUXDB.EXAMPLES-ASYNC::*DB*)
              (COMMAND-FAIL (CL-INFLUXDB.EXAMPLES-ASYNC::E)
               (PRINT CL-INFLUXDB.EXAMPLES-ASYNC::E)))
;;-------------------------------------------------------------------------

Results:

T
```
===========================================================================
 
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
    (format t "Wait for future to come") 
    (loop for i from 1 to 10 
      when (not (fulfilledp p)) do 
        (progn (print i) (sleep .1)))
    (force p))
```
Get it? On to using this pattern with write-points

```
;;-----------------------------------CODE----------------------------------
(LET ((CL-INFLUXDB.EXAMPLES-ASYNC::P (LPARALLEL.PROMISE:PROMISE)))
  (LPARALLEL.PROMISE:FUTURE (LET ((CL-INFLUXDB.EXAMPLES-ASYNC::RESULT
                                   (WRITE-POINTS
                                    CL-INFLUXDB.EXAMPLES-ASYNC::*INFLUXDB*
                                    (LIST
                                     (LIST*
                                      '(:NAME
                                        . CL-INFLUXDB.EXAMPLES-ASYNC::GASRATECO2)
                                      (LIST*
                                       '(:COLUMNS
                                         CL-INFLUXDB.EXAMPLES-ASYNC::INPUTGASRATE
                                         CL-INFLUXDB.EXAMPLES-ASYNC::CO2)
                                       (LIST
                                        (CONS
                                         :POINTS
                                         (DATA-TABLE:ROWS
                                          (CL-CSV:GET-DATA-TABLE-FROM-CSV
                                           (DRAKMA:HTTP-REQUEST
                                            "http://datasets.connectmv.com/file/gas-furnace.csv"
                                            :WANT-STREAM
                                            T))))))))
                                    :TIME-PRECISION
                                    'CL-INFLUXDB.EXAMPLES-ASYNC::S)))
                              (LPARALLEL.PROMISE:FULFILL
                               CL-INFLUXDB.EXAMPLES-ASYNC::P
                               CL-INFLUXDB.EXAMPLES-ASYNC::RESULT)))
  (FORMAT T "Right, lets sleep for a second...~%")
  (SLEEP 1)
  (IF (LPARALLEL.PROMISE:FULFILLEDP CL-INFLUXDB.EXAMPLES-ASYNC::P)
      (FORMAT T "Looks like it's done~%")
      (FORMAT T
              "Looks like it's not done yet. Well we will force it, then~%"))
  (PRINT (LPARALLEL.PROMISE:FORCE CL-INFLUXDB.EXAMPLES-ASYNC::P)))
;;-------------------------------------------------------------------------
Right, lets sleep for a second...
Looks like it's done

T 
Results:

T
```
===========================================================================

Lets try a group by query...asynchronously
We will request fulfillment wait up to 10 seconds and the get the results

```
;;-----------------------------------CODE----------------------------------
(LET ((CL-INFLUXDB.EXAMPLES-ASYNC::P (LPARALLEL.PROMISE:PROMISE)))
  (LPARALLEL.PROMISE:FUTURE (LPARALLEL.PROMISE:FULFILL CL-INFLUXDB.EXAMPLES-ASYNC::P
                                                       (QUERY
                                                        CL-INFLUXDB.EXAMPLES-ASYNC::*INFLUXDB*
                                                        "select max(inputgasrate/co2) from gasrateco2 group by time(160m);")))
  (LOOP CL-INFLUXDB.EXAMPLES-ASYNC::FOR CL-INFLUXDB.EXAMPLES-ASYNC::I
                                        CL-INFLUXDB.EXAMPLES-ASYNC::FROM
                                        1
                                        CL-INFLUXDB.EXAMPLES-ASYNC::TO
                                        1000
        WHEN (NOT (LPARALLEL.PROMISE:FULFILLEDP CL-INFLUXDB.EXAMPLES-ASYNC::P))
          DO (PROGN (FORMAT T
                            "Wait ~fms~%"
                            (/ CL-INFLUXDB.EXAMPLES-ASYNC::I 100))
                    (SLEEP 0.01)))
          (LPARALLEL.PROMISE:FORCE CL-INFLUXDB.EXAMPLES-ASYNC::P))
;;-------------------------------------------------------------------------
Wait 0.01ms
Wait 0.02ms
Wait 0.03ms

Results:

(((:NAME . "gasrateco2") (:COLUMNS "time" "max")
  (:POINTS (1395196800 0.05624))))
```
===========================================================================

Remove the new database 'example-async
```
;;-----------------------------------CODE----------------------------------
(HANDLER-CASE (DELETE-DATABASE
                CL-INFLUXDB.EXAMPLES-ASYNC::*INFLUXDB*
                CL-INFLUXDB.EXAMPLES-ASYNC::*DB*)
              (COMMAND-FAIL (CL-INFLUXDB.EXAMPLES-ASYNC::E)
               (PRINT CL-INFLUXDB.EXAMPLES-ASYNC::E)))
;;-------------------------------------------------------------------------

Results:

T
```
LICENSE
=======
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
