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
(defvar *create-window-lock* (make-recursive-lock))
(defvar *create-window-owned-classes* (make-hash-table))
(defvar *create-window-owned-procedures* (make-hash-table))

(defvar *parent-window-handle-lock* (make-recursive-lock))
(defvar *parent-window-handle* (null-pointer))

(defmacro with-parent-window ((parent-window-name-or-handle &key (class-name parent-window-name-or-handle)) &body body)
  `(with-recursive-lock-held (*parent-window-handle-lock*)
     (let ((*parent-window-handle* (window-p ,parent-window-name-or-handle :class-name ,class-name)))
       (when *parent-window-handle*
	 ,@body))))

(defcallback MainWndProc LRESULT
    ((hWnd   HWND)
     (Msg    :unsigned-int)
     (wParam WPARAM)
     (lParam LPARAM))
  (with-recursive-lock-held (*create-window-lock*)
    (let ((proc (gethash (pointer-address hWnd) *create-window-owned-procedures*)))
      (if proc (funcall proc hWnd Msg wParam lParam))))
  (cond ((eq (window-message-p Msg) :DESTROY)
  	 (post-quit-message 0) 0)
	((eq (window-message-p Msg) :CLOSE)
	 (destroy-window hWnd) 0)
	(t (DefWindowProcA hWnd Msg wParam lParam))))

(defun register-class (class-name
		       &key
			 (procedure (callback MainWndProc))
			 (style '(:HREDRAW :VREDRAW)))
  (with-foreign-object (wnd-class '(:struct WNDCLASSEX))
    
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'cbSize) (foreign-type-size '(:struct WNDCLASSEX)))
    (setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) 'style) style)
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

(defun create-window (window-name
		      &key
			(class-name window-name)
			(procedure nil procedure-p)
			(style +WS_OVERLAPPEDWINDOW+)
			(extended-style +WS_EX_OVERLAPPEDWINDOW+)
			(x +CW_USEDEFAULT+)
			(y +CW_USEDEFAULT+)
			(width +CW_USEDEFAULT+)
			(height +CW_USEDEFAULT+)
			(parent-name-or-handle (null-pointer) parent-name-or-handle-p)
			(parent-class-name parent-name-or-handle))
  (let ((hParentWnd (if parent-name-or-handle-p
			(window-p parent-name-or-handle :class-name parent-class-name)
			parent-name-or-handle)))
    (when hParentWnd
      (when (null-pointer-p (FindWindowExA hParentWnd (null-pointer) class-name window-name))
	(let* ((atom (register-class class-name))
	       (style (if (null-pointer-p hParentWnd) style
			  (remove :POPUP (push :CHILD style))))
	       (hWnd (CreateWindowExA extended-style
				      class-name
				      window-name
				      style
				      x
				      y
				      width
				      height
				      hParentWnd
				      (null-pointer)
				      (GetModuleHandleA (null-pointer))
				      (null-pointer))))
	  (with-recursive-lock-held (*create-window-lock*)
	    (unless (eq atom 0)
	      (if (null-pointer-p hWnd)
		  (unregister-class class-name)
		  (setf (gethash (pointer-address hWnd) *create-window-owned-classes*) class-name)))
	    (unless (null-pointer-p hWnd)
	      (when procedure-p
		(setf (gethash (pointer-address hWnd) *create-window-owned-procedures*) procedure))
	      hWnd)))))))

(defun get-window (window-name-or-handle &key (class-name window-name-or-handle))
  (window-p window-name-or-handle :class-name class-name))

(defun set-parent-window (window-name-or-handle parent-window-name-or-handle &key (class-name window-name-or-handle) (parent-class-name parent-window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name))
	(hParentWnd (window-p parent-window-name-or-handle :class-name parent-class-name)))
    (when (and hWnd hParentWnd)
      (SetParent hWnd hParentWnd))))

(defun get-parent-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (GetParent hWnd))))

(defun get-ancestor-window (window-name-or-handle ga &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (GetAncestor hWnd ga))))

(defun window-p (window-name-or-handle &key (class-name window-name-or-handle))
  (when window-name-or-handle
    (if (pointerp window-name-or-handle) 
	(when (IsWindow window-name-or-handle) window-name-or-handle)
	(when class-name
	  (let ((hWnd (FindWindowExA *parent-window-handle* (null-pointer) class-name window-name-or-handle)))
	    (when (and hWnd (not (null-pointer-p hWnd))) hWnd))))))

(defun get-window-class-name (window-handle)
  (when (window-p window-handle)
    (string-trim " " (with-foreign-pointer-as-string ((class-name class-name-length) 256)
		       (GetClassNameA window-handle class-name class-name-length)))))

(defun get-window-title (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (let ((length (GetWindowTextLengthA hWnd)))
	(unless (eq 0 length)
	  (with-foreign-pointer-as-string (str (incf length))
	    (GetWindowTextA hWnd str length)))))))

(defun set-window-title (window-name-or-handle title &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd (stringp title)
	  (SetWindowTextA hWnd title))))

(defun destroy-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (prog1
	  (DestroyWindow hWnd)
	(with-recursive-lock-held (*create-window-lock*)
	  (remhash (pointer-address hWnd) *create-window-owned-procedures*)
	  (let ((class-name (gethash (pointer-address hWnd) *create-window-owned-classes*)))
	    (when class-name
	      (unregister-class class-name)
	      (remhash (pointer-address hWnd) *create-window-owned-classes*))))))))

(defun show-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (or (window-visible-p hWnd)
	  (and (not (ShowWindow hWnd :SHOWNORMAL)) (window-visible-p hWnd)) ; fixme: 1st call wont work right after system load or reload under SBCL
	  (not (ShowWindow hWnd :SHOWNORMAL))))))

(defun hide-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (or (not (window-visible-p hWnd))
	  (ShowWindow hWnd :HIDE)))))

(defun enable-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (or (window-enabled-p hWnd)
	  (EnableWindow hWnd t)))))

(defun disable-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (or (not (window-enabled-p hWnd))
	  (not (EnableWindow hWnd nil))))))

(defun active-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (or (SetActiveWindow hWnd); fixme: 1st call wont work right after system load or reload under SBCL
	  (SetActiveWindow hWnd)))))

(defun foreground-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (SetForegroundWindow hWnd))))

(defun select-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (SwitchToThisWindow hWnd t)
      t)))

(defun focus-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (SetFocus hWnd))))

(defun minimize-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (CloseWindow hWnd))))

(defun maximize-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (ShowWindow hWnd :MAXIMIZE)
      (window-maximized-p hWnd))))

(defun restore-window (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (ShowWindow hWnd :RESTORE)
      (not (or (window-minimized-p hWnd) (window-maximized-p hWnd))))))

(defun window-enabled-p (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (IsWindowEnabled hWnd))))

(defun window-visible-p (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (IsWindowVisible hWnd))))

(defun window-focused-p (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (pointer-eq hWnd (GetFocus)))))

(defun window-active-p (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (pointer-eq hWnd (GetActiveWindow)))))

(defun window-foregrounded-p (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (pointer-eq hWnd (GetForegroundWindow)))))

(defun window-minimized-p (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (IsIconic hWnd))))

(defun window-maximized-p (window-name-or-handle &key (class-name window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name)))
    (when hWnd
      (IsZoomed hWnd))))

(defun child-window-p (window-name-or-handle parent-window-name-or-handle &key (class-name window-name-or-handle) (parent-class-name parent-window-name-or-handle))
  (let ((hWnd (window-p window-name-or-handle :class-name class-name))
	(hParentWnd (window-p parent-window-name-or-handle :class-name parent-class-name)))
    (when (and hWnd hParentWnd)
      (IsChild hParentWnd hWnd))))

(defun process-message (&optional (window-name-or-handle (null-pointer) window-name-or-handle-p) (extra-process-func nil extra-process-func-p))
  (let ((hWnd (when window-name-or-handle-p (window-p window-name-or-handle))))
    (with-foreign-object (msg '(:struct MSG))
      (with-foreign-object (accelerator-table '(:struct ACCEL))
	(let ((hAccel (CreateAcceleratorTableA accelerator-table 1)))
	  (unless (null-pointer-p hAccel)
	    (loop while (eq 1 (GetMessageA msg (or hWnd window-name-or-handle) 0 0))
	       do (unless (TranslateAcceleratorA (foreign-slot-value msg '(:struct MSG) 'hWnd) hAccel msg)
		    (when extra-process-func-p (funcall extra-process-func (foreign-slot-value msg '(:struct MSG) 'hWnd) msg))
		    (TranslateMessage msg)
		    (DispatchMessageA msg)))))))))

(defun post-quit-message (exit-code)
  (PostQuitMessage exit-code))

(defun window-message-p (message)
  (if (keywordp message)
      (when (foreign-enum-value 'WM_ENUM message :errorp nil)
	message)
      (foreign-enum-keyword 'WM_ENUM message :errorp nil)))

(defun start-window (&rest args)
  (make-thread
   (lambda ()
     (let ((hWnd (apply #'create-window args)))
       (when hWnd
	 (show-window hWnd)
	 (process-message hWnd)
	 (destroy-window hWnd))))))
