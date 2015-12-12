(in-package #:w32api)

;;; "w32api" goes here. Hacks and glory await!

;;; kernel32 - Info
(defmacro with-system-info ((&rest slot-name-and-var-list) &body body)
  (let ((psi (gensym)))
    `(with-foreign-struct ((,psi SYSTEM_INFO) ,@(mapcar #'list slot-name-and-var-list))
       (GetSystemInfo ,psi)
       ,@body)))

(defun get-processor-type ()
  (with-system-info ((:dwProcessorType type)) type))

(defun get-processor-arch ()
  (with-system-info ((:wProcessorArchitecture arch)) arch))

(defun get-processor-count ()
  (with-system-info ((:dwNumberOfProcessors count)) count))

(defun processor-feature-present-p (feature)
  (IsProcessorFeaturePresent feature))

(defmacro with-version-info ((&rest slot-name-and-var-list) &body body)
  (let ((pvi (gensym)) (pvis (gensym)))
    `(with-foreign-struct ((,pvi OSVERSIONINFOEX ,pvis) ,@(mapcar #'list slot-name-and-var-list))
       (setf (foreign-slot-value ,pvi '(:struct OSVERSIONINFOEX) :dwOSVersionInfoSize) ,pvis)
       (when (GetVersionExW ,pvi)
	 ,@body))))

(defun get-os-version ()
  (with-version-info ((:dwMajorVersion major)
		      (:dwMinorVersion minor)
		      (:wServicePackMajor sp-major)
		      (:wServicePackMinor sp-minor))
    (values major minor sp-major sp-minor)))

(defun get-os-build-number ()
  (with-version-info ((:dwBuildNumber build-number))
    build-number))

(defun get-firmware-type ()
  (multiple-value-bind (major minor)
      (get-os-version)
    (if (and (>= major 6) (>= minor 2))
	(with-foreign-object (var 'FIRMWARE_TYPE_ENUM)
	  (when (GetFirmwareType var)
	    (mem-aref var 'FIRMWARE_TYPE_ENUM)))
	:FirmwareTypeBios)))

(defun boot-from-vhd-p ()
  (multiple-value-bind (major minor)
      (get-os-version)
    (when (and (>= major 6) (>= minor 2))
      (with-foreign-object (res :boolean)
	(IsNativeVhdBoot res)
	(mem-ref res :boolean)))))

(defun get-product-type ()
  (multiple-value-bind (major minor sp-major sp-minor)
      (get-os-version)
    (if (>= major 6)
	(with-foreign-object (type 'PRODUCT_ENUM)
	  (when (GetProductInfo major minor sp-major sp-minor type)
	    (mem-ref type 'PRODUCT_ENUM)))
	(with-version-info ((:wProductType type))
	  type))))

(defmacro with-name (api (&rest slot-name-and-var-list) &body body)
  `(let (,@(loop for (slot-name var) in slot-name-and-var-list
	      collect `(,var nil)))
     ,@(loop for (slot-name var) in slot-name-and-var-list
	  collect
	    `(with-foreign-pointer (try 0)
	       (with-foreign-object (size 'DWORD)
		 (setf (mem-ref size 'DWORD) 0)
		 (,api ,slot-name try size)
		 (with-foreign-pointer (try (* 2 (mem-ref size 'DWORD)))
		   (when (,api ,slot-name try size)
		     (setf ,var (foreign-string-to-lisp try)))))))
     ,@body))

(defun get-computer-name (&optional (type :ComputerNameNetBIOS))
  (with-name GetComputerNameExW ((type name))
    name))

(defun get-user-name (&optional (type :NameDisplay))
  (with-name GetUserNameExW ((type name))
    name))

(defun get-error ()
  (GetLastError))

(defun print-error (error-code &key (lang :LANG_NEUTRAL) (sublang :SUBLANG_SYS_DEFAULT))
  (let ((dwFLAGS '(:FORMAT_MESSAGE_ALLOCATE_BUFFER
		   :FORMAT_MESSAGE_ARGUMENT_ARRAY
		   :FORMAT_MESSAGE_FROM_SYSTEM)))
    (with-foreign-object (strptr :pointer)
      (FormatMessageW dwFLAGS
		      (null-pointer)
		      error-code
		      (MAKELANGID lang sublang)
		      strptr
		      0
		      (null-pointer))
      (string-trim (list #\Space #\Tab #\NewLine #\Return)
		   (foreign-string-to-lisp (mem-ref strptr :pointer))))))

(defmacro get-sys-dir (api)
  `(with-foreign-pointer (try 0)
     (let ((size 0))
       (setf size (,api try size))
       (with-foreign-pointer (try (* 2 size))
	 (,api try size)
	 (foreign-string-to-lisp try)))))

(defun get-windows-directory (&key (system-p nil))
  (if system-p
      (get-sys-dir GetSystemWindowsDirectoryW)
      (get-sys-dir GetWindowsDirectoryW)))

(defun get-system-directory ()
  (get-sys-dir GetSystemDirectoryW))

;;; user32 - Desktop
(defun create-desktop (name)
  (when (stringp name)
    (with-foreign-struct ((sa SECURITY_ATTRIBUTES struct-size)
			  (:nLength struct-size)
			  (:lpSecurityDescriptor (null-pointer))
			  (:bInheritHandle t))
      (let ((desktop (CreateDesktopW name
				     (null-pointer)
				     (null-pointer)
				     0
				     w32api.type::+DESKTOP_GENERIC_ALL+
				     sa)))
	(unless (null-pointer-p desktop)
	  desktop)))))

(defun open-desktop (name)
  (when (stringp name)
    (let ((desktop (OpenDesktopW name
				 0
				 t
				 w32api.type::+DESKTOP_GENERIC_ALL+)))
      (unless (null-pointer-p desktop)
	desktop))))

(defun get-current-desktop ()
  (let ((desktop (GetThreadDesktop (GetCurrentThreadId))))
    (unless (null-pointer-p desktop)
      desktop)))

(defcallback EnumDesktopsW :boolean
    ((lpszDesktop :string)
     (lParam LPARAM))
  (declare (ignore lParam) (special desktops))
  (setf desktops (cons lpszDesktop desktops)))
(defun get-all-desktop ()
  (let ((winsta (GetProcessWindowStation))
	(desktops nil))
    (declare (special desktops))
    (unless (null-pointer-p winsta)
      (EnumDesktopsW winsta (callback EnumDesktopsW) 0)
      desktops)))

(defun switch-desktop (desktop)
  (and (pointerp desktop)
       (SetThreadDesktop desktop)
       (SwitchDesktop desktop)))

(defun destroy-desktop (desktop)
  (when (pointerp desktop)
    (CloseDesktop desktop)))

(defmacro with-desktop ((name) &body body)
  (let ((old (gensym))
	(new (gensym)))
    `(let* ((,old (get-current-desktop))
	    (,new (open-desktop ,name))
	    (,new (or ,new (create-desktop ,name))))
       (when (and ,old ,new)
	 (unwind-protect
	      (progn
		(switch-desktop ,new)
		,@body)
	   (when (switch-desktop ,old)
	     (destroy-desktop ,new)))))))

;;; user32 - Window Proc
(defvar *message-handlers* (make-hash-table :test #'equal))

(defun message-handler (window &optional Msg)
  (when (and (window-p window))
    (gethash (cons (pointer-address window) Msg) *message-handlers*)))

(defun message-handler+ (window handler &optional Msg)
  (when (and (window-p window) (functionp handler))
    (setf (gethash (cons (pointer-address window) Msg) *message-handlers*) handler)))

(defun message-handler- (window &optional Msg)
  (if Msg
      (when (and (window-p window))
	(remhash (cons (pointer-address window) Msg) *message-handlers*))
      (maphash (lambda (wm h)
		 (declare (ignore h))
		 (when (eq (first wm) (pointer-address window))
		   (remhash wm *message-handlers*)))
	       *message-handlers*)))

(defcallback MainWndProc LRESULT
    ((hWnd   HWND)
     (Msg    WND_MESSAGE)
     (wParam WPARAM)
     (lParam LPARAM))
  (progn
    (let* ((default-handler (gethash (cons (pointer-address hWnd) nil) *message-handlers* #'DefWindowProcW))
	   (handler         (gethash (cons (pointer-address hWnd) Msg) *message-handlers*))
	   (res             (funcall (or handler default-handler) hWnd Msg wParam lParam)))
      (if (numberp res)
	  res
	  0))))

(defun clear-message (&optional (window (null-pointer)))
  (with-foreign-object (msg '(:struct MSG))
    (loop while (PeekMessageW msg (or (window-p window) (null-pointer)) 0 0 :PM_REMOVE))))

(defun process-message (&optional (window (null-pointer)) (extra-process-func nil extra-process-func-p))
  (with-foreign-object (msg '(:struct MSG))
    (with-foreign-object (accelerator-table '(:struct ACCEL))
      (let ((hAccel (CreateAcceleratorTableW accelerator-table 1)))
	(unless (null-pointer-p hAccel)
	  (loop while (eq 1 (GetMessageW msg (or (window-p window) (null-pointer)) 0 0))
	     do (unless (TranslateAcceleratorW (foreign-slot-value msg '(:struct MSG) :hWnd) hAccel msg)
		  (when extra-process-func-p (funcall extra-process-func (foreign-slot-value msg '(:struct MSG) :hWnd) msg))
		  (TranslateMessage msg)
		  (DispatchMessageW msg))))))))

(defun post-quit-message (exit-code)
  (PostQuitMessage exit-code))

;;; user32 - Window Class
(defvar *window-classes* (make-hash-table))

(defmacro with-class ((name &rest args) &body body)
  `(unless (eq (register-class ,name ,@args) 0)
     (unwind-protect
	  (progn ,@body)
       (unregister-class ,name))))

(defun register-class (name &key
			      (procedure (callback MainWndProc))
			      (style '(:CS_HREDRAW :CS_VREDRAW))
			      (background-color (GetSysColorBrush :COLOR_WINDOW)))
  (if (stringp name)
      (with-foreign-struct ((window-class WNDCLASSEX struct-size)
			    (:cbSize struct-size)
			    (:style  style)
			    (:lpfnWndProc procedure)
			    (:cbClsExtra 0)
			    (:cbWndExtra 0)
			    (:hInstance (GetModuleHandleW (null-pointer)))
			    (:hIcon (null-pointer))
			    (:hCursor (null-pointer))
			    (:hbrBackground background-color)
			    (:lpszMenuName (null-pointer))
			    (:lpszClassName name)
			    (:hIconSm (null-pointer)))
	(RegisterClassExW window-class)) 
      0))

(defun unregister-class (name)
  (when (stringp name)
    (UnregisterClassW name (GetModuleHandleW (null-pointer)))))

;;; user32 - Window
(defvar *parent-window* (null-pointer))

(defmacro with-parent-window ((parent) &body body)
  (let ((p (gensym)))
    `(progn
       (let ((,p ,parent))
	 (when (window-p ,p)
	   (let ((*parent-window-handle* ,p))
	     ,@body))))))

(defmacro with-window ((var &rest args) &body body)
  `(let ((,var (create-window ,@args)))
     (when ,var
       (unwind-protect
	    (progn ,@body)
	 (destroy-window ,var)))))

(defmacro with-windows ((&rest args) &body body)
  (if args
      `(with-window ,(car args)
	 (with-windows ,(cdr args) ,@body))
      `(progn ,@body)))

(defun create-window (name &key
			     (class-name name) 
			     (style +WS_OVERLAPPEDWINDOW+)
			     (extended-style +WS_EX_OVERLAPPEDWINDOW+)
			     (x +CW_USEDEFAULT+)
			     (y +CW_USEDEFAULT+)
			     (width +CW_USEDEFAULT+)
			     (height +CW_USEDEFAULT+)
			     (parent *parent-window*))
  (unless (get-window name :class-name class-name :parent parent)
    (let* ((atom (register-class class-name))
	   (style (if (window-p parent)
		      (remove :WS_POPUP (cons :WS_CHILD style))
		      style))
	   (hWnd (CreateWindowExW extended-style
				  (string class-name)
				  name
				  style
				  x
				  y
				  width
				  height
				  (or (window-p parent) (null-pointer))
				  (null-pointer)
				  (GetModuleHandleW (null-pointer))
				  (null-pointer))))
      (progn
	(unless (eq atom 0)
	  (if (null-pointer-p hWnd)
	      (unregister-class class-name)
	      (setf (gethash (pointer-address hWnd) *window-classes*) class-name)))
	(unless (null-pointer-p hWnd)
	  hWnd)))))

(defun window-p (window)
  (when (and window
	     (pointerp window)
	     (not (null-pointer-p window))
	     (IsWindow window))
    window))

(defun get-window (name &key (class-name name) (parent *parent-window*))
  (let ((hWnd (FindWindowExW
	       (or (window-p parent) (null-pointer))
	       (null-pointer)
	       (string class-name)
	       name)))
    (unless (null-pointer-p hWnd) hWnd)))

(defun set-window-style (window &optional (styles w32api.type:+WS_OVERLAPPEDWINDOW+))
  (when (window-p window)
    (SetWindowStyle window (ensure-list styles))))

(defun get-window-style (window)
  (when (window-p window)
    (GetWindowStyle window)))

(defun set-parent-window (window &optional (parent (null-pointer)))
  (when (and (window-p window) (or (window-p parent) (null-pointer-p parent)))
    (cond ((window-p parent)
	   (set-window-style window (remove :WS_POPUP (cons :WS_CHILD (get-window-style window)))))
	  ((null-pointer-p parent)
	   (set-window-style window (remove :WS_CHILD (cons :WS_POPUP (get-window-style window))))))
    (SetParent window parent)))

(defun get-parent-window (window)
  (when (window-p window)
    (let ((parent (GetParent window)))
      (unless (null-pointer-p parent) parent))))

(defun get-ancestor-window (window ga)
  (when (window-p window)
    (let ((ancestor (GetAncestor window ga)))
      (unless (null-pointer-p ancestor) ancestor))))

(defun get-child-window (window &key (nth 1) (reverse nil))
  (when (and (window-p window) (> nth 0))
    (let* ((child (GetWindow window :GW_CHILD))
	   (child (if reverse (GetWindow child :GW_HWNDLAST) child))
	   (next (if reverse :GW_HWNDPREV :GW_HWNDNEXT)))
      (loop repeat (1- nth)
	 when (and child (not (null-pointer-p child)))
	 do (setf child (GetWindow child next)))
      (unless (null-pointer-p child) child))))

(defun get-children-windows (window)
  (when (window-p window)
    (let* ((child (GetWindow window :GW_CHILD)))
      (loop while (and child (not (null-pointer-p child)))
	 collect (prog1 child
		   (setf child (GetWindow child :GW_HWNDNEXT)))))))

(defcallback EnumChildWindows :boolean
    ((window HWND)
     (lparam LPARAM))
  (declare (ignore lparam) (special descendant))
  (setf descendant (cons window descendant)))
(defun get-descendant-windows (window)
  (when (window-p window)
    (let ((descendant nil))
      (declare (special descendant))
      (EnumChildWindows window (callback EnumChildWindows) 0)
      descendant)))

(defun get-desktop-window ()
  (let ((window (GetDesktopWindow)))
    (unless (null-pointer-p window) window)))

(defun get-window-class-name (window)
  (when (window-p window)
    (with-foreign-pointer-as-string
	(class-name (* 2 +window-class-name-max-length+))
      (GetClassNameW window class-name +window-class-name-max-length+))))

(defun get-window-title (window)
  (when (window-p window)
    (let ((length (GetWindowTextLengthW window)))
      (unless (eq 0 length)
	(with-foreign-pointer-as-string (title (* 2 (incf length)))
	  (GetWindowTextW window title length))))))

(defun set-window-title (window title)
  (when (and (window-p window) (stringp title))
    (SetWindowTextW window title)))

(defun destroy-window (window)
  (when (window-p window)
    (prog1
	(DestroyWindow window)
      (progn
	(message-handler- window)
	(let ((class-name (gethash (pointer-address window) *window-classes*)))
	  (when class-name
	    (unregister-class class-name)
	    (remhash (pointer-address window) *window-classes*)))))))

(defun show-window (window)
  (when (window-p window)
    (or (window-visible-p window)
	(and (not (ShowWindow window :SW_SHOWNORMAL)) (window-visible-p window)) ; fixme: 1st call wont work right after system load or reload under SBCL
	(not (ShowWindow window :SW_SHOWNORMAL)))))

(defun hide-window (window)
  (when (window-p window)
    (or (not (window-visible-p window))
	(ShowWindow window :SW_HIDE))))

(defun enable-window (window)
  (when (window-p window)
    (or (window-enabled-p window)
	(EnableWindow window t))))

(defun disable-window (window)
  (when (window-p window)
    (or (not (window-enabled-p window))
	(not (EnableWindow window nil)))))

(defun active-window (window)
  (when (window-p window)
    (or (SetActiveWindow window); fixme: 1st call wont work right after system load or reload under SBCL
	(SetActiveWindow window))))

(defun foreground-window (window)
  (when (window-p window)
    (SetForegroundWindow window)))

(defun switch-window (window)
  (when (window-p window)
    (SwitchToThisWindow window t)
    t))

(defun focus-window (window)
  (when (window-p window)
    (SetFocus window)))

(defun minimize-window (window)
  (when (window-p window)
    (CloseWindow window)))

(defun maximize-window (window)
  (when (window-p window)
    (ShowWindow window :SW_MAXIMIZE)
    (window-maximized-p window)))

(defun restore-window (window)
  (when (window-p window)
    (ShowWindow window :SW_RESTORE)
    (and (not (window-minimized-p window))
	 (not (window-maximized-p window)))))

(defun move-window (window x y width height)
  (when (window-p window)
    (MoveWindow window x y width height t)))

(defun invalidate-rect (window x1 y1 x2 y2 &optional (erase-p t))
  (when (window-p window)
    (with-foreign-struct ((rect RECT)
			  (:left x1)  (:top  y1)
			  (:right x2) (:bottom y2))
      (InvalidateRect window rect erase-p))))

(defun validate-rect (window x1 y1 x2 y2)
  (when (window-p window)
    (with-foreign-struct ((rect RECT)
			  (:left x1)  (:top  y1)
			  (:right x2) (:bottom y2))
      (ValidateRect window rect))))

(defun update-window (window)
  (when (window-p window)
    (UpdateWindow window)))

(defmacro arrange-window (api parent windows how range)
  `(when (or (window-p ,parent) (null-pointer-p ,parent))
     (let* ((count (length ,windows))
	    (windows (remove-if-not #'window-p ,windows))
	    (real-count (length ,windows)))
       (when (/= 0 (if (= count 0)
		       (print (,api (print ,parent) ,how ,range 0 (null-pointer)))
		       (with-foreign-object (children :pointer real-count)
			 (loop for index from 0 below real-count
			    do (setf (mem-aref children :pointer index) (nth index ,windows)))
			 (print (,api (print ,parent) ,how ,range real-count children)))))
	 t))))

(defun tile-windows (&optional (how :MDITILE_ZORDER) (parent (null-pointer)) windows)
  (arrange-window TileWindows parent windows how (null-pointer)))

(defun cascade-windows (&optional (how :MDITILE_HORIZONTAL) (parent (null-pointer)) windows)
  (arrange-window CascadeWindows parent windows how (null-pointer)))

(defun window-enabled-p (window)
  (and (window-p window)
       (IsWindowEnabled window)))

(defun window-visible-p (window)
  (and (window-p window)
       (IsWindowVisible window)))

(defun window-focused-p (window)
  (and (window-p window)
       (pointer-eq window (GetFocus))))

(defun window-active-p (window)
  (and (window-p window)
       (pointer-eq window (GetActiveWindow))))

(defun window-foregrounded-p (window)
  (and (window-p window)
       (pointer-eq window (GetForegroundWindow))))

(defun window-minimized-p (window)
  (and (window-p window)
       (IsIconic window)))

(defun window-maximized-p (window)
  (and (window-p window)
       (IsZoomed window)))

(defun parent-window-p (window parent)
  (and (window-p window)
       (window-p parent)
       (IsChild parent window)))

;;; user32 - Window Control
(defun create-button (name window &key
				    (x 0)
				    (y 0)
				    (width 100)
				    (height 30)
				    (style nil)
				    (default-p nil))
  (let* ((button (create-window name
				:class-name :BUTTON
				:style (append '(:WS_TABSTOP :WS_VISIBLE :WS_CHILD :BS_PUSHBUTTON) style (when default-p '(:BS_DEFPUSHBUTTON)))
				:parent window
				:x x
				:y y
				:width width
				:height height))
	 (BTNDEFPROC (make-pointer (GetWindowLongPtrW button :GWLP_WNDPROC))))

    ;; Subclassing
    (message-handler+ button
		    (lambda (hWnd Msg wParam lParam)
		      (CallWindowProcW BTNDEFPROC hWnd Msg wParam lParam)))
    
    (SetWindowLongPtrW button :GWLP_WNDPROC (pointer-address (callback MainWndProc)))
    button))

(defun create-checkbox (name window &key
				      (x 0)
				      (y 0)
				      (width 100)
				      (height 30)
				      (style nil)
				      (default-p nil))
  (declare (inline))
  (create-button name window
		 :x x
		 :y y
		 :width width
		 :height height
		 :style (append '(:BS_AUTOCHECKBOX) style)
		 :default-p default-p))

;;; Editbox
(defun create-input (name window &key
				   (x 0)
				   (y 0)
				   (width 150)
				   (height 30)
				   (style nil))
  (let* ((editor (create-window name
				:class-name :EDIT
				:style (append '(:WS_VISIBLE :WS_CHILD :ES_LEFT) style)
				:parent window
				:x x
				:y y
				:width width
				:height height))
	 (EDITDEFPROC (make-pointer (GetWindowLongPtrW editor :GWLP_WNDPROC))))

    ;; Subclassing
    (message-handler+ editor
		      (lambda (hWnd Msg wParam lParam)
			(CallWindowProcW EDITDEFPROC hWnd Msg wParam lParam)))
    
    (SetWindowLongPtrW editor :GWLP_WNDPROC (pointer-address (callback MainWndProc)))
    editor))

(defun create-editor (name window &key
				    (x 0)
				    (y 0)
				    (width 400)
				    (height 300)
				    (style nil))
  (declare (inline))
  (create-input name window
		:x x
		:y y
		:width width
		:height height
		:style (append '(:WS_VISIBLE :WS_CHILD :WS_VSCROLL :ES_MULTILINE :ES_AUTOVSCROLL) style)))

;;; gdi32 - Drawing
(defun get-window-rectangle (window &optional client-area-p)
  (when (window-p window)
    (with-foreign-struct ((rect RECT)
			  ((:left x1))
			  ((:top  y1))
			  ((:right x2))
			  ((:bottom y2)))
      (if client-area-p
	  (GetClientRect window rect)
	  (GetWindowRect window rect))
      (values x1 y1 x2 y2))))

(defun get-window-size (window &optional client-area-p)
  (when (window-p window)
    (multiple-value-bind (x1 y1 x2 y2)
	(get-window-rectangle window client-area-p)
      (values (- x2 x1)
	      (- y2 y1)))))

(defun get-drawing-context (window &key (full nil))
  (when (window-p window)
    (if full
	(GetDC window)
	(GetWindowDC window))))

(defun get-drawing-context-window (dc)
  (and dc
       (pointerp dc)
       (not (null-pointer-p dc))
       (WindowFromDC dc)))

;; (defun begin-drawing-context (window)
;;   (when (window-p window)
;;     (let ((paint (foreign-alloc '(:struct PAINTSTRUCT))))
;;       (values (BeginPaint window paint) paint))))

;; (defun end-drawing-context (window paint)
;;   (when (and (window-p window)
;; 	     (pointerp paint)
;; 	     (not (null-pointer-p paint)))
;;     (and (EndPaint window paint)
;; 	 (foreign-free paint))))

(defun call-with-drawing-context (window &optional draw-func)
  (when (and (functionp draw-func) (window-p window))
    (with-foreign-object (paint '(:struct PAINTSTRUCT))
      (let ((dc (BeginPaint window paint)))
	(unless (null-pointer-p dc)
	  (unwind-protect
	       (funcall draw-func dc)
	    (EndPaint window paint)))))))

(defmacro with-drawing-context ((var window) &body draws)
  `(call-with-drawing-context ,window (lambda (,var) ,@draws)))

;;;

(defun create-window-thread (name &rest args)
  (let ((finish (make-condition-variable))
	(cv-lock (make-recursive-lock))
	(output-stream *standard-output*))
    (prog1
	(make-thread
	 (lambda ()
	   (let ((*standard-output* output-stream))
	     (let ((hWnd (apply #'create-window name args)))
	       (condition-notify finish)
	       (when hWnd
		 (message-handler+ hWnd :WM_DESTROY (lambda (hWnd Msg wParam lParam)
						      (declare (ignore hWnd Msg wParam lParam))
						      (post-quit-message 0)))
		 (message-handler+ hWnd :WM_CLOSE   (lambda (hWnd Msg wParam lParam)
						      (declare (ignore Msg wParam lParam))
						      (destroy-window hWnd)))
		 
		 (unwind-protect
		      (progn (show-window hWnd)
			     (process-message))
		   (destroy-window hWnd)))
	       ))))
      (acquire-lock cv-lock)
      (condition-wait finish cv-lock))))

(defmacro with-window-thread ((thread) &body body)
  (let ((output-stream (gensym)))
    `(let ((,output-stream *standard-output*))
       (interrupt-thread
	,thread
	(lambda ()
	  (let ((*standard-output* ,output-stream))
	    ,@body)
	  (thread-yield))))))
