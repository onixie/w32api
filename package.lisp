;;;; package.lisp

(defpackage #:w32api
  (:use #:cl #:cffi #:bordeaux-threads #:lparallel
	#:w32api.type
	#:w32api.kernel32
	#:w32api.secur32	
	#:w32api.user32
	#:w32api.gdi32
	#:w32api.util)
  (:export get-processor-type
	   get-processor-arch
	   get-processor-count
	   processor-feature-present-p
	   get-firmware-type
	   boot-from-vhd-p
	   get-product-type
	   get-os-version
	   get-os-build-number
	   get-computer-name
	   get-user-name
	   get-windows-directory
	   get-system-directory
	   get-error
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
	   start-window

	   update-window
	   create-button
	   create-input
	   create-editor
	   with-drawing-context
	   get-drawing-context
	   get-drawing-context-window
	   get-window-size
	   get-window-rectangle
	   move-window
	   ))
