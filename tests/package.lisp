(defpackage :cl-influxdb.tests
  (:use :cl
	:cl-influxdb
	)
  (:export :run-tests)
  )

(in-package :cl-influxdb.tests)

(defun run-tests ()
  (let ((*print-pretty* t))
    (nst:nst-cmd :run-package #.*package*)
    ))
