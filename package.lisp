;;;; package.lisp

(defpackage #:cl-influxdb
  (:use #:cl
        #:cl-json
        #:drakma
        #:cl-annot
        #:cl-annot.class
        #:flexi-streams
        )
  (:import-from #:do-urlencode
   :urlencode)
  )

(defpackage #:cl-influxdb-v8
  (:use #:cl
        #:cl-json
        #:drakma
        #:cl-annot
        #:cl-annot.class
        #:flexi-streams
        )
  (:import-from #:do-urlencode
   :urlencode)
  )

