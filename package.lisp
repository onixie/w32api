;;;; package.lisp

(defpackage #:w32api
  (:use #:cl #:cffi
	#:w32api.type
	#:w32api.kernel32
	#:w32api.user32
	#:w32api.gdi32)
  (:export get-error
	   print-error
	   register-class
	   unregister-class
	   create-window
	   show-window
	   hide-window
	   destroy-window))
