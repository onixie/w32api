;;;; package.lisp

(defpackage #:w32api
  (:use #:cl #:cffi #:bordeaux-threads #:lparallel
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
	   get-window-title
	   set-window-title
	   show-window
	   hide-window
	   enable-window
	   disable-window
	   focus-window
	   active-window
	   foreground-window
	   destroy-window
	   window-enabled-p
	   window-visible-p
	   child-window-p
	   window-focused-p
	   window-active-p
	   window-foregrounded-p
	   process-message
	   post-quit-message
	   window-message-p
	   start-window))
