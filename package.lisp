;;;; package.lisp

(defpackage #:w32api
  (:use #:cl #:cffi #:bordeaux-threads
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
	   format-error
	   print-error

	   get-all-monitors
	   get-monitor
	   get-window-monitor
	   get-monitor-name
	   get-monitor-rectangle
	   create-desktop
	   open-desktop
	   switch-desktop
	   destroy-desktop
	   get-default-desktop
	   get-current-desktop
	   get-window-desktop
	   get-all-desktops
	   desktop-p
	   get-desktop-name
	   register-class
	   unregister-class
	   with-class
	   create-window
	   window-p
	   get-window
	   find-window
	   with-window
	   with-windows
	   get-owner-window
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
	   raise-window
	   bury-window
	   destroy-window
	   tile-windows
	   cascade-windows
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
	   message-handler+
	   message-handler-
	   update-window
	   invalidate-rect
	   validate-rect
	   create-button
	   create-input
	   create-editor
	   with-drawing-context
	   get-drawing-context
	   get-drawing-context-window
	   get-window-size
	   get-window-rectangle
	   get-update-rectangle
	   move-&-resize-window
	   move-window
	   resize-window
	   raise-window
	   bury-window
	   wm-command-handler
	   proc
	   create-button
	   create-checkbox
	   create-radiobox
	   create-groupbox
	   create-input
	   create-editor

	   make-rgb-color
	   get-color-rgb

	   get-key-character
	   get-key-repeat-count
	   key-released-p
	   key-pressed-p
	   
	   get-cursor-x
	   get-cursor-y
	   get-modifier-key

	   get-text-height
	   get-text-ascent
	   get-text-descent
	   get-text-char-width
	   get-text-extent
	   
	   with-drawing-object
	   with-background-mode
	   get-text-color
	   set-text-color
	   get-background-color
	   set-background-color

	   create-font
	   destroy-font
	   get-current-font
	   get-font-info
	   get-all-font-infos))
