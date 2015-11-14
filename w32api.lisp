(in-package #:w32api)

;;; "w32api" goes here. Hacks and glory await!

;;; kernel32
(defun get-error ()
  (GetLastError))

(defun print-error (error-code)
  (let ((dwFLAGS (logior  #X00001000 #X00000200 #X00000100)))
    (with-foreign-object (strptr :pointer)
      (FormatMessageA dwFLAGS (null-pointer) error-code 0 strptr 0 (null-pointer))
      (foreign-string-to-lisp (mem-ref strptr :pointer)))))

;;; user32
(defcallback MyWndProc LRESULT
    ((hWnd   HWND)
     (Msg   :unsigned-int)
     (wParam WPARAM)
     (lParam LPARAM))
  (print wParam)
  (print lParam)
  (DefWindowProcA hWnd Msg wParam lParam))

(defun register-class (class-name)
  (with-foreign-object (wnd-class '(:struct WNDCLASSEX))
    
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'cbSize) (foreign-type-size '(:struct WNDCLASSEX)))
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'style) (logior 1 2))
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'lpfnWndProc) (callback MyWndProc))
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'cbClsExtra) 0)
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'cbWndExtra) 0)
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'hInstance) (GetModuleHandleA (null-pointer)))
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'hIcon) (null-pointer))
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'hCursor) (null-pointer))
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'hbrBackground) (null-pointer))
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'lpszMenuName) (null-pointer))
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'lpszClassName) class-name)
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'hIconSm) (null-pointer))

    (RegisterClassExA wnd-class)))

(defun unregister-class (class-name)
  (UnregisterClassA class-name (GetModuleHandleA (null-pointer))))

(defun create-window (window-name &key (class-name window-name))
  (when (null-pointer-p (FindWindowExA (null-pointer) (null-pointer) class-name window-name))
    (register-class class-name)
    (let ((hWnd (CreateWindowExA
		 +WS_EX_OVERLAPPEDWINDOW+
		 class-name
		 window-name
		 +WS_OVERLAPPEDWINDOW+
		 +CW_USERDEFAULT+
		 +CW_USERDEFAULT+
		 +CW_USERDEFAULT+
		 +CW_USERDEFAULT+
		 (null-pointer)
		 (null-pointer)
		 (GetModuleHandleA (null-pointer))
		 (null-pointer))))
      (unless (null-pointer-p hWnd)
	(not (ShowWindow hWnd :HIDE))))))

(defun show-window (window-name &key (class-name window-name))
  (let ((hWnd (FindWindowExA (null-pointer) (null-pointer) class-name window-name)))
    (unless (null-pointer-p hWnd)
      (not (ShowWindow hWnd :SHOWNORMAL)))))

(defun hide-window (window-name &key (class-name window-name))
  (let ((hWnd (FindWindowExA (null-pointer) (null-pointer) class-name window-name)))
    (unless (null-pointer-p hWnd)
      (ShowWindow hWnd :HIDE))))

(defun destroy-window (window-name &key (class-name window-name))
  (let ((hWnd (FindWindowExA (null-pointer) (null-pointer) class-name window-name)))
    (unless (null-pointer-p hWnd)
      (DestroyWindow hWnd))))

(defun process-message (window-name &key (class-name window-name))
  (let ((hWnd (FindWindowExA (null-pointer) (null-pointer) class-name window-name)))
    (unless (null-pointer-p hWnd)
      (with-foreign-object (msg '(:struct MSG))
	(with-foreign-object (accelerator-table '(:struct ACCEL))
	  (let ((hAccel (CreateAcceleratorTableA accelerator-table 1)))
	    (unless (null-pointer-p hAccel)
	      (loop while (GetMessageA msg (null-pointer) 0 0)
		   do (unless (TranslateAcceleratorA (foreign-slot-value msg '(:struct MSG) 'hWnd) hAccel msg)
			(TranslateMessage msg)
			(DispatchMessageA msg))))))))))
