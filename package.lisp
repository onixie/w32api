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
	   get-window
	   window-p
	   get-window-class-name
	   show-window
	   hide-window
	   enable-window
	   disable-window
	   active-window
	   destroy-window
	   window-enabled-p
	   window-visible-p
	   child-window-p
	   window-active-p
	   process-message))
