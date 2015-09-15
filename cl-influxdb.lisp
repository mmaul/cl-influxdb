(in-package #:cl-influxdb)
(annot:enable-annot-syntax)


;ok .9
@export-class
(defclass influxdb ()
  ((host :accessor influxdb.host :initarg :host :initform "127.0.0.1")
   (port :accessor influxdb.port :initarg :port :initform 8086)
   (user :accessor influxdb.user :initarg :user :initform "root")
   (password :accessor influxdb.password :initarg :password :initform "root")
   (database :accessor influxdb.database :initarg :database)
   (retention-policy :accessor influxdb.retention-policy :initarg :retention-policy :initform nil)
   (write-consistency :accessor influxdb.write-consistency :initarg :write-consistency :initform nil)
   (reuse-connection :accessor influxdb.reuse-connection :initarg :reuse-connection :initform nil)
   (headers :accessor influxdb.headers 
	    :initform '(:Content-type  "application/json"
			:Accept "text/plain"))
   (baseurl :accessor influxdb.baseurl)
   (cookie-jar :accessor influxdb.cookie-jar :initform (make-instance 'drakma:cookie-jar))
   (stream :accessor influxdb.stream :initform nil)
   )
  (:documentation "InfluxDB connection"))

;ok .9
(defmethod initialize-instance :after 
           ((self influxdb) &key) 
  (setf (influxdb.baseurl self) 
	(format nil "http://~a:~d" (influxdb.host self) (influxdb.port self))
        ))
@export
(define-condition invalid-time-precision (error)
  ((text :initarg :text :reader text))
  )

@export
(define-condition command-fail (error)
  ((body-or-stream :initarg :body-or-stream :reader body) 
   (status-code :initarg :status-code :reader status-code) 
   (headers :initarg :headers :reader headers) 
   (uri :initarg :uri :reader uri) 
   (stream :initarg :stream :reader cmd-stream) 
   (must-close :initarg :must-close :reader must-close) 
   (reason-phrase :initarg :reason-phrase :reader reason-phrase))
  (:report (lambda (c s) (format s "INITDB COMMAND-FAIL: ~a, ~a" 
				 (reason-phrase c) (body c))))
  )

;ok .9
(defun symbol-keyword-or-string (v)
  "Takes symbol keyword or string and returns a string representation
   In the case of symbols and keywords the returned string is down cased."
  (etypecase v
     (KEYWORD (string-downcase 
	       (symbol-name v)))
     (SYMBOL (string-downcase 
	      (symbol-name v)))
     (STRING v)
     ))

;ok .9
(defun assert-valid-time-precision (tval)
  (when (not (or (string= tval "u") (string= tval "u") (string= tval "m") (string= tval "s") (string= tval "ms")))
      (error 'invalid-time-precision :text 
		      (format nil "Time precision must be one of s m u"))
    )
)

;ok .9
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
              while pos)))

;ok .9
(defun influxdb-key-tag-fmt (v)
  (let ((w (symbol-keyword-or-string v)))
    (replace-all (replace-all w " " "\\ ") "," "\\,")))

;ok .9
(defun influxdb-value-fmt (v)
  (etypecase v
    (integer (format nil "~ai" v))
    (real (format nil "~f" v))
    (string (format nil "\"~a\"" (replace-all v "\"" "\\\"")))
    (keyword (format nil "\"~a\"" (symbol-keyword-or-string v)))
    (symbol (format nil "\"~a\"" (symbol-keyword-or-string v)))
    (boolean (format nil "~a" v))
    )
  )

;ok .9
@export-structure
(defstruct influxdb-data key tags columns points)
(make-influxdb-data :key :a :tags (sort '((:host . "server1") (:aregion . "one")) (lambda (a b) (string< (car a) (car b)))))

;ok.9
(defun encode-influxdb-data (data)
  (let ((key-and-tags (format nil "~{~A~^,~}"
                              (cons (influxdb-key-tag-fmt (influxdb-data-key data))
                                    (sort  (mapcar (lambda (v) (print v) (format nil "~a=~a"
                                                                  (influxdb-key-tag-fmt (car v))
                                                                  (influxdb-key-tag-fmt (cdr v))))
                                                   (influxdb-data-tags data))
                                           #'string<)))))
    (loop for point in (influxdb-data-points data)
          collect (let ((line
                          (format nil "~A ~{~A~^,~}"
                                  key-and-tags
                                  (mapcar (lambda (k v) (format nil "~a=~a" 
                                                           (influxdb-key-tag-fmt k)
                                                           (influxdb-value-fmt v)))
                                          (influxdb-data-columns data) point
                                          ))))
                    (if (> (length point) (length (influxdb-data-columns data)))
                        (format nil "~a ~a" line (car (reverse point)))
                        line)
                    ))
    )
  )

;ok .9
(defmethod influxdb-cmd ((self influxdb) arg-list &key (data ()) (params ()) (method :post) (ok-status-code 200) debug)
  "Submits influxdb command defined by ARG-LIST which the contains elements of
   the path after after the base influxdb URL. Certain commands require a 
   specific HTTP method method should be one of :GET :POST :DELETE. 
   Thd DATA argument should be a lisp object representation of a JSON
   object. See examples.lisp write-response-times and refer to CL-JSON 
   documentation for ENCODE-TO-STRING. PARAMS is an alist of paramater
   which will be encoded in the HTTP request (they do not need to be url-encoded).

   Returns multiple values CONTENT, REASON
   On failure raises COMMAND-FAIL condition
  "
  (let ((args (mapcar #'symbol-keyword-or-string arg-list)) (reuse-connection (influxdb.reuse-connection self) )
        (content
          (typecase data
            (list (encode-json-to-string data))
            (INFLUXDB-DATA (format nil "~{~A~^~%~}" (encode-influxdb-data data)))
            (t data)))
        )
    (when debug (print content))
    (when debug (print (format nil "~a/~{~A~^/~}" 
                               (influxdb.baseurl self) args)))
    (multiple-value-bind (body-or-stream status-code headers uri stream 
					 must-close reason-phrase) 
	(drakma:http-request (format nil "~a/~{~A~^/~}" 
				     (influxdb.baseurl self) args)
			     :method method
			     :parameters (acons "u" (influxdb.user self)
						(acons "p" (influxdb.password self)
						       params)
					      )
			                        
			     :content content
			     :stream (if reuse-connection
					 (influxdb.stream self)
					 nil)
			     :close (if reuse-connection nil t)
			     :cookie-jar (influxdb.cookie-jar self))
      (setf (influxdb.stream self) 
	    (if (and reuse-connection (not must-close))
		stream
		nil))
      (when (and reuse-connection must-close)
	(close stream))
      (if (= ok-status-code status-code) 
	  (progn (when debug 
		   (pprint (list content body-or-stream status-code headers uri stream 
				must-close reason-phrase))
		   
		   )
		 (values (etypecase body-or-stream
			   ((SIMPLE-ARRAY (UNSIGNED-BYTE 8))
			    (let ((resp (flexi-streams:octets-to-string body-or-stream)))
                  (if (string= "" resp) nil (json:decode-json-from-string resp))))
			   ((VECTOR (UNSIGNED-BYTE 8))
                (let ((resp (flexi-streams:octets-to-string body-or-stream)))
                  (if (string= "" resp) nil (json:decode-json-from-string (flexi-streams:octets-to-string body-or-stream)))))
			   (string (json:decode-json-from-string body-or-stream))
			   (t (if body-or-stream body-or-stream t))
			   ) 
			 reason-phrase))
	  (error 'command-fail 
		 :body-or-stream body-or-stream 
		 :status-code status-code 
		 :headers headers 
		 :uri uri 
		 :stream stream 
		 :must-close must-close 
		 :reason-phrase reason-phrase)
	  )
      
      )))



;;; 
;;; Database Interaction Commands
;;;

;ok .9
@export
(defmethod close-reuseable-connection ((self influxdb))
  "When using :resue-connection this connection should be called to close the
   stream. If :reuse-connection is not set then this is unnecessary.
  "
  (let ((stream (influxdb.stream self))) 
    (when (and stream (open-stream-p stream)) 
      (close stream)))
  )

;ok .9
@export
(defmethod write-points ((self influxdb) data &key (time-precision "s") write-consistency retention-policy debug)
  "
  data is lisp representation of JSON object of the form
  (
  (
   :name: <series name>
   :columns (<list of column names)
   :points: (
     (<row value list 1>)
     ...
     (<row value listn>)
   )
  )
 ) 

 Time precision should be one of 
   'u' microsecond
   's' second
   'm' miniute
 When TIME-PRECISION is not of 's' 'm' 'u' 'ms'or 'us' INVALID-TIME-PRECISION is invoked
 On Failure COMMAND-FAIL condtion is invoked
 On Success Values T and a reason STRING are areturned
 "
  (let ((time-precision-string (symbol-keyword-or-string time-precision)))
    (assert-valid-time-precision time-precision-string)
    (influxdb-cmd self '("write")
                  :data data :params
                  (let ((params (acons "db" (influxdb.database self) ())))
                    (when (or write-consistency (influxdb.write-consistency self))
                      (setf params (acons "consistency"
                                         (or write-consistency (influxdb.write-consistency self)) params)))
                    (when (or retention-policy (influxdb.retention-policy self))
                      (setf params (acons "rp"
                                         (or retention-policy (influxdb.retention-policy self)) params)))
                    params
                    )
                  :ok-status-code 204
			       :debug debug
			       )) 
  )
  

;ok .9
@export
(defmethod query ((self influxdb) query-txt &key (time-precision "s") 
					      (chunked nil) retention-policy debug)
  "
 When TIME-PRECISION is not of 'n' 's' 'm' or 'u' INVALID-TIME-PRECISION is invoked
 On Failure COMMAND-FAIL condtion is invoked
 On Success Values lisp representation of JSON and a reason STRING are areturned
  "
  (let ((time-precision-string (symbol-keyword-or-string time-precision)))
    (assert-valid-time-precision time-precision-string)
    (influxdb-cmd self (list "query")
                  :params (let ((params
                                  (acons "chunked" chunked
                                         (acons "precision" time-precision-string 
                                                (acons "q" query-txt
                                                       (acons "db" (influxdb.database self) ()))))))
                            (when (or retention-policy (influxdb.retention-policy self))
                              (setf params (acons "rp"
                                                  (or retention-policy (influxdb.retention-policy self)) params)))
                            params)
		  :method :get :debug debug))
  )
 
;ok .9
@export
(defun get-series (query-results)
  "Transforms results from query into an alist with keys :status :name :columns :values"
  (assert (> (length query-results) 0))
  (let ((results-list  (car query-results)))
    (mapcar (lambda (x)
              (let ((y (car x)))
                (cond
                  ((equal :series (car y)) (cons '(:status :ok) (cadr y)))
                  ((equal :error (car y)) (list (cons :status (car y))
                                                (cons :description  (cdr y))))
                  (t (list '(:status . :undefined))))))
            (cdr results-list ))))
;ok .9
@export
(defmethod create-database ((self influxdb) database)
  "Creates a database named DATABASE. 

  Returns multiple values CONTENT, REASON, STREAM
  On failure raises COMMAND-FAIL condition
"
  
  (values t (nth-value 1 
                       (influxdb-cmd self '("query") :method :get
                                                     :params (acons  "q" (concatenate 'string  "CREATE DATABASE " database) '()) :ok-status-code 200)))
  )

;ok .9
@export
(defmethod delete-database ((self influxdb) database)
  "deletes a database named DATABASE. 

  Returns multiple values CONTENT, REASON, STREAM
  On failure raises COMMAND-FAIL condition
"
  (query self (concatenate 'string  "drop database " database))
  
  )

;ok .9
@export
(defmethod get-database-list ((self influxdb))
  "
  Returns list of defined databases
  On Failure COMMAND-FAIL condtion is invoked
  "
  (query self "show databases;")
  )


;ok .9
@export
(defmethod ping ((self influxdb))
  "
   healthcheck
  "
  (influxdb-cmd self (list "ping" ) :method :get :ok-status-code 204))

;ok .9
@export
(defmethod list-servers ((self influxdb))
  "
    cluster config endpoints
  "
  (query self "show servers"))

;;;
;;; Cluster and User Admin Section
;;;

;ok .9
@export 
(defmethod switch-database ((self influxdb) database)
  (setf (influxdb.database self) database))

;ok .9
@export 
(defmethod switch-user ((self influxdb) user password)
  (setf (influxdb.user self) user)
  (setf (influxdb.password self) password))

;ok .9
@export
(defmacro print-run (header &rest code )
  `(progn  (format t (concatenate 'string  
    "===========================================================================~%"	
	   ,header "~%```~%;;-----------------------------------CODE----------------------------------" ))
	   (pprint ',@code) (format t "~%;;-------------------------------------------------------------------------~%")
	   (let ((results (eval ',@code))) 
	     (format t "~%Results:~%")
	     (pprint results) (format t "~%```~%") results)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
