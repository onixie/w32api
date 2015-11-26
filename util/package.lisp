(defpackage #:w32api.util
  (:use #:cl #:cffi #:bordeaux-threads)
  (:export bitfield-union
	   enum-union
	   ensure-list))
