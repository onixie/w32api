(defpackage #:w32api.util
  (:use #:cl #:cffi #:bordeaux-threads)
  (:export ensure-list
	   make-keyword
	   bitfield-union
	   enum-union
	   with-foreign-struct
	   parse-foreign-struct
	   with-foreign-union))
