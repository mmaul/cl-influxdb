;;;; cl-influxdb.lisp

(in-package #:cl-influxdb)
(annot:enable-annot-syntax)

@export-class
(defclass influxdb ()
  ((host :accessor influxdb.host :initarg :host :initform "127.0.0.1")
   (port :accessor influxdb.port :initarg :port :initform 8086)
   (user :accessor influxdb.user :initarg :user :initform "root")
   (password :accessor influxdb.password :initarg :password :initform "root")
   (database :accessor influxdb.database :initarg :database)
   (reuse-connection :accessor influxdb.reuse-connection :initarg :reuse-connection :initform nil)
   (headers :accessor influxdb.headers 
	    :initform '(:Content-type  "application/json"
			:Accept "text/plain"))
   (baseurl :accessor influxdb.baseurl)
   (cookie-jar :accessor influxdb.cookie-jar :initform (make-instance 'drakma:cookie-jar))
   (stream :accessor influxdb.stream :initform nil)
   )
  (:documentation "InfluxDB connection"))

(defmethod initialize-instance :after 
           ((self influxdb) &rest args)
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
   (stream :initarg :stream :reader stream) 
   (must-close :initarg :must-close :reader must-close) 
   (reason-phrase :initarg :reason-phrase :reader reason-phrase))
  (:report (lambda (c s) (format s "INITDB COMMAND-FAIL: ~a, ~a" 
				 (reason-phrase c) (body c))))
  )

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

(defun assert-valid-time-precision (tval)
  (when (not (or (string= tval "u") (string= tval "m") (string= tval "s")))
      (error 'invalid-time-precision :text 
		      (format nil "Time precision must be one of s m u"))
    )
)

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
	(content (if data (encode-json-to-string data) nil))
	)
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
			    (json:decode-json-from-string 
			     (flexi-streams:octets-to-string body-or-stream))
			    )
			   ((VECTOR (UNSIGNED-BYTE 8))
			    (json:decode-json-from-string 
			     (flexi-streams:octets-to-string body-or-stream))
			    )
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

@export
(defmethod close-reuseable-connection ((self influxdb))
  "When using :resue-connection this connection should be called to close the
   stream. If :reuse-connection is not set then this is unnecessary.
  "
  (let ((stream) (influxdb.stream self)) 
    (when (and stream (open-stream-p stream)) 
      (close stream)))
  )

@export
(defmethod write-points ((self influxdb) data &key (time-precision "s") debug)
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
    (influxdb-cmd self (list "db" (influxdb.database self) "series")
			       :data data :params (acons "time-precision" 
							 time-precision-string ())
			       :debug debug
			       )) 
  )
  

@export
(defmethod query ((self influxdb) query-txt &key (time-precision "s") 
					      (chunked nil) debug)
  "
 When TIME-PRECISION is not of 's' 'm' or 'u' INVALID-TIME-PRECISION is invoked
 On Failure COMMAND-FAIL condtion is invoked
 On Success Values lisp representation of JSON and a reason STRING are areturned
  "
  (let ((time-precision-string (symbol-keyword-or-string time-precision)))
    (assert-valid-time-precision time-precision-string)
    (influxdb-cmd self (list "db" (influxdb.database self) "series")
		  :params (acons "chunked" chunked
				 (acons "time_precision" time-precision-string 
					(acons "q" query-txt ())))
		  :method :get :debug debug))
  )

@export
(defmethod delete-series ((self influxdb) series)
  "

  "
  (values t (nth-value 1 
		       (influxdb-cmd self (list :db (influxdb.database self)  :series series) 
			    :method :delete :ok-status-code 204)))
  )

@export
(defmethod create-database ((self influxdb) database)
  "Creates a database named DATABASE. 

  Returns multiple values CONTENT, REASON, STREAM
  On failure raises COMMAND-FAIL condition
"
  
  (values t (nth-value 1 
	      (influxdb-cmd self '("db") :data (list (cons 'name  database)) :ok-status-code 201)))
  )

@export
(defmethod delete-database ((self influxdb) database)
  "deletes a database named DATABASE. 

  Returns multiple values CONTENT, REASON, STREAM
  On failure raises COMMAND-FAIL condition
"
  
  (values t (nth-value 1 
	      (influxdb-cmd self (list "db" database) 
			    :method :delete :ok-status-code 204)))
  )

@export
(defmethod get-database-list ((self influxdb))
  "
  Returns list of defined databases
  On Failure COMMAND-FAIL condtion is invoked
  "
  (influxdb-cmd self (list "db") :method :get)
  )


;;;
;;; continuous queries management interface
;;;

@export
(defmethod list-continous-queries ((self influxdb))
  (influxdb-cmd self (list "db" (influxdb.database self) "continous_queries") 
		:method :get :ok-status-code 200))


(defmethod create-continous-queries ((self influxdb) query)
  (influxdb-cmd self (list "db" (influxdb.database self) "continous_queries") 
		:method :post :data (acons "query" query ()) :ok-status-code 200))


@export
(defmethod delete-continous-queries ((self influxdb) id)
  (values t (nth-value 1 
	      (influxdb-cmd self (list "db" (influxdb.database self) "continous_queries" id) 
			    :method :delete :ok-status-code 200)))
  )


@export
(defmethod ping ((self influxdb))
  "
   healthcheck
  "
  (influxdb-cmd self (list "ping" ) :method :get :ok-status-code 200))



(defmethod force-raft-compaction ((self influxdb))
  "
   force a raft log compaction
  "
  (influxdb-cmd self (list "raft" "force_compaction") :method :post 
		:ok-status-code 200))


@export
(defmethod interfaces ((self influxdb))
  "
   fetch current list of available interfaces
  "
  (influxdb-cmd self (list "interfaces" ) :method :get :ok-status-code 200))


@export
(defmethod list-servers ((self influxdb))
  "
    cluster config endpoints
  "
  (influxdb-cmd self (list "cluster" "servers") :method :get :ok-status-code 200))
@export
(defmethod create-shard ((self influxdb) shard)
  (influxdb-cmd self (list "cluster" "shards") :method :post 
		:ok-status-code 200 :data shard))
@export
(defmethod get-shards ((self influxdb))
  (influxdb-cmd self (list "cluster" "shards") :method :get :ok-status-code 200))
@export
(defmethod drop-shard ((self influxdb) id)
  (values t (nth-value 1 
	      (influxdb-cmd self (list "cluster" "shards" id) 
			    :method :delete :ok-status-code 200))))

;;;
;;; Cluster and User Admin Section
;;;

@export 
(defun switch-database ((self influxdb) database)
  (setf (influxdb.database self) database))

@export 
(defun switch-user ((self influxdb) user password)
  (setf (influxdb.user self) user)
  (setf (influxdb.password self) password))


@export
(defmethod get-list-cluster-admins ((self influxdb))
  (influxdb-cmd self (list "cluster_admins") :method :get)
  )

@export
(defmethod add-cluster-admin ((self influxdb) user password)
  (values t (nth-value 1 
	      (influxdb-cmd self '("cluster_admins") 
			    :data (list (cons 'name  user)
					(cons 'password password)
					)
			    :method :post :ok-status-code 200)))
  )

@export
(defmethod update-cluster-admin-password ((self influxdb) password)
  (values t (nth-value 1 
	      (influxdb-cmd self '("cluster_admins") 
			    :data (list (cons 'password password)
					)
			    :method :post)))
  )

@export
(defmethod delete-cluster-admin ((self influxdb) user)
  (values t (nth-value 1 
	      (influxdb-cmd self (list "cluster_admins" user) 
			    :method delete :ok-status-code 204)))
  )

@export
(defmethod alter-database-admin ((self influxdb) user is-admin)
  (values t (nth-value 1 
	      (influxdb-cmd self (list "db" (influxdb.database "users" user)) 
			    :data (list (cons 'admin is_admin))
			    :method post)))
  )

@export
(defmethod set-database-admin ((self influxdb) user)
  (alter-database-admin self user t))

@export
(defmethod unset-database-admin ((self influxdb) user)
  (alter-database-admin self user nil))

@export 
(defun get-database-users ((self influxdb) )
  (influxdb-cmd self (list "db" (influxdb.database) users) :method :get))

@export
(defmethod add-database-user ((self influxdb) user password)
  (values t (nth-value 1 
	      (influxdb-cmd self (list "db" (influxdb.database self) "users") 
			    :data (list (cons 'name  user)
					(cons 'password password)
					)
			    :method :post))))

@export 
(defmethod update-database-user-password ((self influxdb) password &optional user)
  "Changes database user password. If no user is spe3cified the current
   users password is change the new password is also updated in the influxdb
   instance.
"
  (let ((resp (nth-value 1 
	      (influxdb-cmd self (list "db" (influxdb.database self)
				       "users" (if user user (influxdb.user self))) 
			    :data (list (cons 'password password))
			    :method :post))))
    (when (not user)
      (setf (influxdb.password self) password)
      )
    (values t resp)
    )
  )

@export 
(defmethod delete-database-user ((self influxdb) user)
  (values t (nth-value 1 
	      (influxdb-cmd self (list "db" (influxdb.database self) 
				       "users" user) 
			    :method :delete))))

@export
(defmacro print-run (header &rest code )
  `(progn  (format t (concatenate 'string  
    "===========================================================================~%"	
	   ,header "~%;;-----------------------------------CODE----------------------------------" ))
	   (pprint ',@code) (format t "~%;;-------------------------------------------------------------------------~%")
	   (let ((results (eval ',@code))) 
	     (format t "~%Results:")
	     (pprint results) (format t "~%") results)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
