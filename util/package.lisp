(defpackage #:w32api.util
  (:use #:cl #:cffi #:bordeaux-threads)
  (:export bitfield-union
	   enum-union
	   ensure-list
	   with-foreign-struct
	   with-foreign-union))
