;;;; package.lisp

(defpackage #:w32api
  (:use #:cl #:cffi #:bordeaux-threads #:lparallel
	#:w32api.type
	#:w32api.kernel32
	#:w32api.user32
	#:w32api.gdi32
	#:w32api.util)
  (:export get-error
	   print-error
	   create-desktop
	   switch-desktop
	   destroy-desktop
	   get-current-desktop
	   register-class
	   unregister-class
	   with-class
	   create-window
	   window-p
	   get-window
	   with-window
	   set-parent-window
	   get-parent-window
	   get-ancestor-window
	   get-desktop-window
	   with-parent-window
	   get-child-window
	   get-children-windows
	   get-descendant-windows
	   get-window-class-name
	   get-window-style
	   set-window-style
	   get-window-title
	   set-window-title
	   show-window
	   hide-window
	   enable-window
	   disable-window
	   switch-window
	   focus-window
	   active-window
	   foreground-window
	   maximize-window
	   minimize-window
	   restore-window
	   destroy-window
	   window-enabled-p
	   window-visible-p
	   parent-window-p
	   window-focused-p
	   window-active-p
	   window-foregrounded-p
	   window-minimized-p
	   window-maximized-p
	   process-message
	   post-quit-message
	   window-message-p
	   window-message-eq
	   start-window

	   update-window
	   create-button
	   with-drawing-context
	   get-drawing-context
	   get-drawing-context-window
	   ))
