(defpackage :cl-influxdb.doc
  (:use :cl))
(in-package :cl-influxdb.doc)
(annot:enable-annot-syntax)

(defun slurp-spit (pth file)
  (with-open-file (f-stream (make-pathname :directory pth :name file) :direction :input)
      (let ((seq (make-string (file-length f-stream))))
	(read-sequence seq f-stream)
	(write-sequence seq o-stream))
      )
  )
@export
(defun make-doc (pth)
  (with-open-file (*standard-output* 
		   (make-pathname :directory pth :name "README.md") 
		   :direction :output 
				     :if-exists :supersede)
    (flet ((slurp-spit (pth file)
	     (with-open-file (f-stream (make-pathname :directory pth :name file) 
				       :direction :input)
	       (let ((seq (make-string (file-length f-stream))))
		 (read-sequence seq f-stream)
		 (write-sequence seq *standard-output*))
	       )
	     ))
      (slurp-spit pth "README.txt") 
      (cl-influxdb.examples:exercise)
      
      ) 
    
    ))
