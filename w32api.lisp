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
(defvar *create-window-owned-classes* (make-hash-table))
(defvar *create-window-owned-procedures* (make-hash-table))

(defcallback WndProc LRESULT
    ((hWnd   HWND)
     (Msg   :unsigned-int)
     (wParam WPARAM)
     (lParam LPARAM))
  (funcall
   (gethash (pointer-address hWnd) *create-window-owned-procedures*
	    (lambda (hWnd Msg wParam lParam)
	      (declare (ignore hWnd Msg wParam lParam))))
   hWnd Msg wParam lParam)
  (DefWindowProcA hWnd Msg wParam lParam))

(defun register-class (class-name &key (procedure (callback WndProc)))
  (with-foreign-object (wnd-class '(:struct WNDCLASSEX))
    
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'cbSize) (foreign-type-size '(:struct WNDCLASSEX)))
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'style) '(:HREDRAW :VREDRAW))
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'lpfnWndProc) procedure)
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

(defun create-window (window-name &key (class-name window-name) (procedure nil procedure-p))
  (when (null-pointer-p (FindWindowExA (null-pointer) (null-pointer) class-name window-name))
    (let* ((atom (register-class class-name))
	   (hWnd (CreateWindowExA +WS_EX_OVERLAPPEDWINDOW+
				  class-name
				  window-name
				  +WS_OVERLAPPEDWINDOW+
				  +CW_USEDEFAULT+
				  +CW_USEDEFAULT+
				  +CW_USEDEFAULT+
				  +CW_USEDEFAULT+
				  (null-pointer)
				  (null-pointer)
				  (GetModuleHandleA (null-pointer))
				  (null-pointer))))
      (unless (eq atom 0)
	(if (null-pointer-p hWnd)
	    (unregister-class class-name)
	    (setf (gethash (pointer-address hWnd) *create-window-owned-classes*) class-name)))
      (unless (null-pointer-p hWnd)
	(when procedure-p
	  (setf (gethash (pointer-address hWnd) *create-window-owned-procedures*) procedure))
	(not (ShowWindow hWnd :HIDE))
	hWnd))))

(defun get-window (window-name &key (class-name window-name))
  (let ((hWnd (FindWindowExA (null-pointer) (null-pointer) class-name window-name)))
    (unless (null-pointer-p hWnd)
      hWnd)))

(defun window-p (window-name-or-handle &key (class-name window-name-or-handle))
  (when (and window-name-or-handle class-name)
    (if (pointerp window-name-or-handle) 
	(when (IsWindow window-name-or-handle) window-name-or-handle)
	(let ((hWnd (get-window window-name-or-handle :class-name class-name)))
	  (when (and hWnd (not (null-pointer-p hWnd))) hWnd)))))

(defun get-window-class-name (window-handle)
  (when (window-p window-handle)
    (string-trim " " (with-foreign-pointer-as-string ((class-name class-name-length) 256)
		       (GetClassNameA window-handle class-name class-name-length)))))

(defun destroy-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (prog1
	  (DestroyWindow hWnd)
	(remhash (pointer-address hWnd) *create-window-owned-procedures*)
	(let ((class-name (gethash (pointer-address hWnd) *create-window-owned-classes*)))
	  (when class-name
	    (unregister-class class-name)
	    (remhash (pointer-address hWnd) *create-window-owned-classes*)))))))

(defun show-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (not (ShowWindow hWnd :SHOWNORMAL)))))

(defun hide-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (ShowWindow hWnd :HIDE))))

(defun enable-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (EnableWindow hWnd t))))

(defun disable-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (not (EnableWindow hWnd nil)))))

(defun active-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (SetActiveWindow hWnd))))

(defun window-enabled-p (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (IsWindowEnabled hWnd))))

(defun window-visible-p (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (IsWindowVisible hWnd))))

(defun child-window-p (window-name-or-handle parent-window-name-or-handle &key (class-name window-name-or-handle) (parent-class-name parent-window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name))
	(hParentWnd (window-p parent-window-name-or-handle :class-name parent-class-name)))
    (when (and hWnd hParentWnd)
      (IsChild hParentWnd hWnd))))

(defun window-active-p (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (pointer-eq hWnd (GetActiveWindow)))))

	   ;; SetForegroundWindow
	   ;; GetForegroundWindow

(defun process-message (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (with-foreign-object (msg '(:struct MSG))
	(with-foreign-object (accelerator-table '(:struct ACCEL))
	  (let ((hAccel (CreateAcceleratorTableA accelerator-table 1)))
	    (unless (null-pointer-p hAccel)
	      (loop while (GetMessageA msg (null-pointer) 0 0)
		 do (unless (TranslateAcceleratorA (foreign-slot-value msg '(:struct MSG) 'hWnd) hAccel msg)
		      (TranslateMessage msg)
		      (DispatchMessageA msg))))))))))
