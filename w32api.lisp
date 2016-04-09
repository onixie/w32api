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
  (let ((try (gensym))
	(size (gensym)))
    `(let (,@(loop for (slot-name var) in slot-name-and-var-list collect `(,var nil)))
       ,@(loop for (slot-name var) in slot-name-and-var-list
	    collect
	      `(with-foreign-pointer (,try 0)
		 (with-foreign-object (,size 'DWORD)
		   (setf (mem-ref ,size 'DWORD) 0)
		   (,api ,slot-name ,try ,size)
		   (unless (zerop (mem-ref ,size 'DWORD))
		     (with-foreign-pointer (,try (* 2 (mem-ref ,size 'DWORD)))
		       (when (,api ,slot-name ,try ,size)
			 (setf ,var (foreign-string-to-lisp ,try))))))))
       ,@body)))

(defun get-computer-name (&optional (type :ComputerNameNetBIOS))
  (with-name GetComputerNameExW ((type name))
    name))

(defun get-user-name (&optional (type :NameSamCompatible))
  (with-name GetUserNameExW ((type name))
    name))

(defun get-error ()
  (GetLastError))

(defun format-error (error-code &key (lang :LANG_NEUTRAL) (sublang :SUBLANG_SYS_DEFAULT))
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

(defun print-error (&rest args)
  (let* ((error-code (get-error))
	 (error-message (apply #'format-error error-code args)))
    (print error-message)
    (values error-code error-message)))

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

;;; low-level snapshot enumeration macro
(defmacro with-th32-snapshot ((portion slot-name-and-var-list &key (snapshots +TH32CS_SNAPALL+) (process-id 0)) &body body)
  (let* ((hSnapshot (gensym))
	 (entry (gensym))
	 (entry-size (gensym))
	 (p32First (case portion
		     (:PROCESS  `(Process32FirstW ,hSnapshot ,entry))
		     (:THREAD   `(Thread32First   ,hSnapshot ,entry))
		     (:MODULE   `(Module32First   ,hSnapshot ,entry))
		     (:HEAPLIST `(Heap32ListFirst ,hSnapshot ,entry))))
	 (p32Next  (case portion
		     (:PROCESS  `(Process32NextW ,hSnapshot ,entry))
		     (:THREAD   `(Thread32Next   ,hSnapshot ,entry))
		     (:MODULE   `(Module32Next   ,hSnapshot ,entry))
		     (:HEAPLIST `(Heap32ListNext ,hSnapshot ,entry))))
	 (p32Entry (case portion
		     (:PROCESS  'PROCESSENTRY32)
		     (:THREAD   'THREADENTRY32)
		     (:MODULE   'MODULEENTRY32)
		     (:HEAPLIST 'HEAPLIST32))))
    (if (eq portion :HEAP)
	(let ((th32HeapID    (gensym))
	      (th32ProcessID (gensym)))
	  `(with-th32-snapshot (:HEAPLIST (((:th32HeapID ,th32HeapID))
					   ((:th32ProcessID ,th32ProcessID))) :process-id ,process-id)
	     (with-foreign-struct ((,entry HEAPENTRY32 ,entry-size) (:dwSize ,entry-size)
				   ,@slot-name-and-var-list)
	       (when (Heap32First ,entry ,th32ProcessID ,th32HeapID)
		 (cons (progn ,@body)
		       (loop while (Heap32Next ,entry) collect (progn ,@body)))))))
	`(let ((,hSnapshot (CreateToolhelp32Snapshot (list ,@snapshots) ,process-id)))
	   (unwind-protect
		(WITH-FOREIGN-struct ((,entry ,p32Entry ,entry-size) (:dwSize ,entry-size)
				      ,@slot-name-and-var-list)
		  (when ,p32First
		    (cons (progn ,@body)
			  (loop while ,p32Next collect (progn ,@body)))))
	     (CloseHandle ,hSnapshot))))))

;;; user32 - Monitor
(defcallback EnumDisplayMonitorsCallback :boolean
    ((hMonitor HMONITOR)
     (hdcMonitor HDC)
     (lprcMonitor (:pointer (:struct RECT)))
     (dwData   LPARAM))
  (declare (special monitors) (ignore hdcMonitor dwData lprcMonitor))
  (setf monitors (cons hMonitor monitors)))
(defun get-all-monitors ()
  (let ((monitors nil))
    (declare (special monitors))
    (EnumDisplayMonitors (null-pointer) (null-pointer) (callback EnumDisplayMonitorsCallback) 0)
    monitors))

(defun get-monitor (&key (x1 0) (y1 0) (x2 nil) (y2 nil) (method nil))
  (let ((monitor
	 (if (and x2 y2)
	     (with-foreign-struct ((rect RECT)
				   (:left x1)
				   (:top y1)
				   (:right x2)
				   (:bottom y2))
	       (MonitorFromRect rect (or method :MONITOR_DEFAULTTONULL)))
	     (with-foreign-struct ((point POINT)
				   (:x x1)
				   (:y y1))
	       ;; checkme: :MONITOR_DEFAULTTONULL always return null?
	       (MonitorFromPoint point (or method :MONITOR_DEFAULTTONEAREST))))))
    (unless (null-pointer-p monitor)
      monitor)))

(defun get-window-monitor (window &optional (method :MONITOR_DEFAULTTONULL))
  (when (window-p window)
    (let ((monitor (MonitorFromWindow window method)))
      (unless (null-pointer-p monitor)
	monitor))))

(defmacro with-monitor-info ((monitor &rest slot-name-and-var-list) &body body)
  (let ((mon-info (gensym))
	(info-size (gensym)))
    `(when (pointerp ,monitor)
       (with-foreign-struct ((,mon-info MONITORINFOEX ,info-size)
			     (:cbSize ,info-size)
			     ,@slot-name-and-var-list)
	 (when (GetMonitorInfoW ,monitor ,mon-info)
	   ,@body)))))

(defun get-monitor-name (monitor)
  (with-monitor-info (monitor ((:szDevice szDevice)))
    (foreign-string-to-lisp szDevice)))

(defun get-monitor-rectangle (monitor &optional (area :rcMonitor))
  (with-monitor-info (monitor ((area rect)))
    (values
     (getf rect :left)
     (getf rect :top)
     (getf rect :right)
     (getf rect :bottom))))

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

(defun get-default-desktop ()
  (let ((desktop nil))
    (join-thread
     (make-thread
      (lambda ()
	(setf desktop (get-current-desktop)))))))

(defun get-window-desktop (window)
  (when (window-p window)
    (let ((desktop (GetThreadDesktop (GetWindowThreadId window))))
      (unless (null-pointer-p desktop)
	desktop))))

(defcallback EnumDesktopsW :boolean
    ((lpszDesktop :string)
     (lParam LPARAM))
  (declare (ignore lParam) (special desktops))
  (setf desktops (cons lpszDesktop desktops)))
(defun get-all-desktops ()
  (let ((winsta (GetProcessWindowStation))
	(desktops nil))
    (declare (special desktops))
    (unless (null-pointer-p winsta)
      (EnumDesktopsW winsta (callback EnumDesktopsW) 0)
      desktops)))

(defun desktop-p (desktop)
  (when (pointerp desktop)
    (with-foreign-object (need 'DWORD)
      (setf (mem-ref need 'DWORD) 0)
      (GetUserObjectInformationW desktop :UOI_TYPE (null-pointer) 0 need)
      (let ((size (mem-ref need 'DWORD)))
	(unless (zerop size)
	  (when (string-equal
		 "desktop"
		 (with-foreign-pointer-as-string (type size)
		   (GetUserObjectInformationW desktop :UOI_TYPE type size need))))
	  desktop)))))

(defun get-desktop-name (desktop)
  (when (pointerp desktop)
    (with-foreign-object (need 'DWORD)
      (setf (mem-ref need 'DWORD) 0)
      (GetUserObjectInformationW desktop :UOI_NAME (null-pointer) 0 need)
      (let ((size (mem-ref need 'DWORD)))
	(unless (zerop size)
	  (with-foreign-pointer-as-string (name size)
	    (GetUserObjectInformationW desktop :UOI_NAME name size need)))))))

(defun switch-desktop (desktop)
  (let ((current (get-current-desktop)))
    (and (pointerp desktop)
	 (SwitchDesktop desktop)
	 (or (SetThreadDesktop desktop)
	     (SwitchDesktop current))
	 current)))

(defun destroy-desktop (desktop)
  (when (pointerp desktop)
    (CloseDesktop desktop)))

(defmacro with-desktop ((name) &body body)
  (let ((old (gensym))
	(new (gensym)))
    `(let* ((,old (get-current-desktop))
	    (,new (or (open-desktop ,name)
		      (create-desktop ,name))))
       (if (and ,old ,new (not (pointer-eq ,old ,new)))
	   (unwind-protect
		(progn
		  (switch-desktop ,new)
		  ,@body)
	     (mapc #'destroy-window (get-descendant-windows (get-desktop-window)))
	     (SetThreadDesktop ,old)	;
	     (destroy-desktop ,new)
	     (switch-desktop ,old))
	   (progn ,@body)))))

;;; user32 - Window Proc
(defvar *message-handlers* (make-hash-table :test #'equal))
(defvar *message-handlers-lock* (make-recursive-lock))

(defun message-handler (window &optional Msg)
  (unless (null-pointer-p window)
    (with-recursive-lock-held (*message-handlers-lock*)
      (let* ((default  (gethash (cons (pointer-address window) t) *message-handlers*))
	     (fallback (gethash (cons (pointer-address window) nil) *message-handlers* #'DefWindowProcW))
	     (handler  (gethash (cons (pointer-address window) Msg) *message-handlers*)))
	(if default
	    (lambda (hWnd Msg wParam lParam)
	      (when handler
		(funcall handler hWnd Msg wParam lParam))
	      (funcall default hWnd Msg wParam lParam))
	    (or handler fallback))))))

(defun message-handler+ (window Msgs handler)
  (when (and (window-p window) (functionp handler))
    (with-recursive-lock-held (*message-handlers-lock*)
      (loop for Msg in (ensure-list Msgs) do
	   (setf (gethash (cons (pointer-address window) Msg) *message-handlers*) handler)))))

(defun message-handler- (window &optional Msg)
  (unless (null-pointer-p window)
    (with-recursive-lock-held (*message-handlers-lock*)
      (if Msg
	  (remhash (cons (pointer-address window) Msg) *message-handlers*)
	  (maphash (lambda (wm h)
		     (declare (ignore h))
		     (when (eq (first wm) (pointer-address window))
		       (remhash wm *message-handlers*)))
		   *message-handlers*)))))

(defmacro proc (&body body)
  (let ((args (gensym)))
    `(lambda (&rest ,args)
       (declare (ignore ,args))
       ,@body)))

(defmacro wm-command-handler ((window control notification-code) &body body)
  (let ((Msg (gensym))
	(wParam (gensym))
	(lParam (gensym)))
    `(lambda (,window ,Msg ,wParam ,lParam)
       (when (eq ,Msg :WM_COMMAND)
	 (let ((,control (if (zerop ,lParam)
			     (LOWORD ,wParam) ;Menu, Accel
			     (make-pointer ,lParam)))
	       (,notification-code (if (zerop ,lParam)
				       (HIWORD ,wParam)
				       (foreign-enum-keyword 'BN_ENUM (HIWORD ,wParam)))))
	   ,@body)))))

(defcallback MainWndProc LRESULT
    ((hWnd   HWND)
     (Msg    WND_MESSAGE)
     (wParam WPARAM)
     (lParam LPARAM))
  (let ((res (funcall (message-handler hWnd Msg) hWnd Msg wParam lParam)))
    (if (numberp res)
	res
	0)))

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
(defvar *window-classes-lock* (make-recursive-lock))

(defun window-class (window)
  (with-recursive-lock-held (*window-classes-lock*)
    (gethash (pointer-address window) *window-classes*)))

(defun window-class+ (window class)
  (with-recursive-lock-held (*window-classes-lock*)
    (setf (gethash (pointer-address window) *window-classes*) class)))

(defun window-class- (window)
  (with-recursive-lock-held (*window-classes-lock*)
    (remhash (pointer-address window) *window-classes*)))

(defmacro with-class ((name procedure &rest args) &body body)
  `(unless (eq (register-class ,name ,procedure ,@args) 0)
     (unwind-protect
	  (progn ,@body)
       (unregister-class ,name))))

(defun register-class (name procedure &key style background-color)
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
			    (:hbrBackground (or background-color (null-pointer)))
			    (:lpszMenuName (null-pointer))
			    (:lpszClassName name)
			    (:hIconSm (null-pointer)))
	(RegisterClassExW window-class))
      0))

(defun unregister-class (name)
  (when (stringp name)
    (UnregisterClassW name (GetModuleHandleW (null-pointer)))))

(defun set-window-class-background-color (window background-color)
  (when (window-p window)
    (SetClassLongPtrW window :GCLP_HBRBACKGROUND (pointer-address background-color))))

;;; user32 - Window
(defvar *parent-window* (null-pointer))

(defmacro with-parent-window ((parent) &body body)
  (let ((p (gensym)))
    `(progn
       (let ((,p ,parent))
	 (when (window-p ,p)
	   (let ((*parent-window* ,p))
	     ,@body))))))

(defmacro with-window ((var &rest args) &body body)
  `(let ((,var (%create-window ,@args)))
     (when ,var
       (unwind-protect
	    (progn ,@body)
	 (destroy-window ,var)))))

(defmacro with-windows ((&rest args) &body body)
  (if args
      `(with-window ,(car args)
	 (with-windows ,(cdr args) ,@body))
      `(progn ,@body)))

(defun %create-window (name &key
			      (style (list* :WS_CLIPSIBLINGS :WS_CLIPCHILDREN +WS_OVERLAPPEDWINDOW+))
			      (extended-style +WS_EX_OVERLAPPEDWINDOW+)
			      (x +CW_USEDEFAULT+)
			      (y +CW_USEDEFAULT+)
			      (width +CW_USEDEFAULT+)
			      (height +CW_USEDEFAULT+)
			      (parent *parent-window*)
			      (owner (null-pointer))
			      (allow-same-name-p nil)
			      ;; argument to register-class if need
			      (class-name name)
			      (procedure (callback MainWndProc))
			      (class-style '(:CS_HREDRAW :CS_VREDRAW))
			      (background-color (GetSysColorBrush :COLOR_WINDOW))
			      &allow-other-keys)
  (unless (and (not allow-same-name-p)
	       (get-window name :class-name class-name :parent parent :current-thread-window-p t))
    (let* ((atom (register-class class-name procedure :style class-style :background-color background-color))
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
				  (or (window-p parent) (window-p owner) (null-pointer))
				  (null-pointer)
				  (GetModuleHandleW (null-pointer))
				  (null-pointer))))
      (progn
	(unless (eq atom 0)
	  (if (null-pointer-p hWnd)
	      (unregister-class class-name)
	      (window-class+ hWnd class-name)))
	(unless (null-pointer-p hWnd)
	  hWnd)))))

(defun create-window (name &rest args)
  (if (window-p (getf args :parent))
      (eval-in-window (getf args :parent) (lambda () (apply #'%create-window name args)))
      (let ((finish (make-condition-variable))
	    (cv-lock (make-recursive-lock))
	    (output-stream *standard-output*)
	    (window nil))
	(make-thread
	 (lambda ()
	   (let ((*standard-output* output-stream))
	     (if (window-p (getf args :owner))
		 (SetThreadDesktop (GetThreadDesktop (GetWindowThreadId (getf args :owner))))
		 (when (pointerp (getf args :desktop))
		   (SetThreadDesktop (getf args :desktop))))
	     (setf window (apply #'%create-window name args))
	     (condition-notify finish)
	     (when window
	       (message-handler+ window :WM_DESTROY (proc (post-quit-message 0)))
	       (message-handler+ window :WM_CLOSE   (proc (destroy-window window)))
	       (unwind-protect
		    (progn
		      (show-window window)
		      (process-message))
		 (destroy-window window))))))
	(acquire-lock cv-lock)
	(condition-wait finish cv-lock)
	window)))

(defun eval-in-window (window thunk)
  (let ((res nil))
    (message-handler+ window :UM_EVAL (proc (setf res (funcall thunk))
					    (message-handler- window :UM_EVAL)))
    (SendMessageW window :UM_EVAL 0 0)
    res))

(defun window-p (window)
  (when (and window
	     (pointerp window)
	     (not (null-pointer-p window))
	     (IsWindow window))
    window))

(defcallback EnumThreadWindowsCallback :boolean
    ((hWnd HWND)
     (lParam LPARAM))
  (declare (special windows) (ignore lParam))
  (setf windows (cons hWnd windows)))
(defun get-current-thread-windows ()
  (let ((windows nil))
    (declare (special windows))
    (when (EnumThreadWindows (GetCurrentThreadId) (callback EnumThreadWindowsCallback) 0)
      windows)))

(defun get-window (name &key (class-name nil) (parent *parent-window*) (nth 0) (current-thread-window-p nil))
  (let ((windows (remove-if-not
		  (lambda (window)
		    (and (string-equal name (get-window-title window))
			 (or (not (stringp class-name)) (string-equal class-name (get-window-class-name window)))
			 (or (not (window-p parent)) (parent-window-p window parent))))
		  (if current-thread-window-p
		      (let ((current-thread-windows (get-current-thread-windows)))
			(append current-thread-windows (mapcan #'get-descendant-windows current-thread-windows)))
		      (get-descendant-windows (get-desktop-window))))))
    (values (nth nth windows) (length windows))))

(defun find-window (name &key (class-name name) (parent *parent-window*))
  (let ((hWnd (FindWindowExW
	       (or (window-p parent) (null-pointer))
	       (null-pointer)
	       (string class-name)
	       name)))
    (unless (null-pointer-p hWnd)
      hWnd)))

(defun set-window-style (window &optional (styles w32api.type:+WS_OVERLAPPEDWINDOW+))
  (when (window-p window)
    (SetWindowStyle window (ensure-list styles))))

(defun get-window-style (window)
  (when (window-p window)
    (GetWindowStyle window)))

(defun set-parent-window (window &optional (parent *parent-window*))
  (when (and (window-p window) (or (window-p parent) (null-pointer-p parent)))
    (cond ((window-p parent)
	   (set-window-style window (remove :WS_POPUP (cons :WS_CHILD (get-window-style window)))))
	  ((null-pointer-p parent)
	   (set-window-style window (remove :WS_CHILD (cons :WS_POPUP (get-window-style window))))))
    (SetParent window parent)))

(defun get-owner-window (window)
  (when (window-p window)
    (let ((owner (GetWindow window :GW_OWNER)))
      (unless (null-pointer-p owner) owner))))

(defun get-ancestor-window (window ga)
  (when (window-p window)
    (let ((ancestor (GetAncestor window ga)))
      (unless (null-pointer-p ancestor) ancestor))))

(defun get-parent-window (window)
  (declare (inline))
  (get-ancestor-window window :GA_PARENT))

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

(defun %destroy-window (window)
  (when (window-p window)
    (prog1
	(and (every #'%destroy-window (get-children-windows window))
	     (DestroyWindow window))
      (progn
	(message-handler- window)
	(let ((class-name (window-class window)))
	  (when class-name
	    (unregister-class class-name)
	    (window-class- window)))))))

(defun destroy-window (window)
  (when (window-p window)
    (if (eq (GetWindowThreadId window) (GetCurrentThreadId))
	(%destroy-window window)
	(eval-in-window window (lambda () (%destroy-window window))))))

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

(defun %focus-window (window)
  (when (window-p window)
    (SetFocus window)))

(defun focus-window (window)
  (if (eq (GetWindowThreadId window) (GetCurrentThreadId))
      (%focus-window window)
      (eval-in-window window (lambda ()(%focus-window window)))))

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

(defun move-&-resize-window (window x y width height)
  (when (window-p window)
    (MoveWindow window x y width height t)))

(defun move-window (window x y)
  (when (window-p window)
    (SetWindowPos window :HWND_TOP x y 0 0 '(:SWP_NOSIZE :SWP_NOZORDER))))

(defun resize-window (window width height)
  (when (window-p window)
    (SetWindowPos window :HWND_TOP 0 0 width height '(:SWP_NOMOVE :SWP_NOZORDER))))

(defun raise-window (window &optional (top-most-p nil))	;checkme: is there any good method?
  (when (window-p window)
    (SetWindowPos window :HWND_TOPMOST 0 0 0 0 '(:SWP_NOMOVE :SWP_NOSIZE))
    (unless top-most-p
      (SetWindowPos window :HWND_NOTOPMOST 0 0 0 0 '(:SWP_NOMOVE :SWP_NOSIZE)))
    (SetWindowPos window :HWND_TOP 0 0 0 0 '(:SWP_NOMOVE :SWP_NOSIZE))))

(defun bury-window (window)
  (when (window-p window)
    (SetWindowPos window :HWND_BOTTOM 0 0 0 0 '(:SWP_NOMOVE :SWP_NOSIZE :SWP_NOACTIVATE))))

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
		       (,api ,parent ,how ,range 0 (null-pointer))
		       (with-foreign-object (children :pointer real-count)
			 (loop for index from 0 below real-count
			    do (setf (mem-aref children :pointer index) (nth index ,windows)))
			 (,api ,parent ,how ,range real-count children))))
	 t))))

(defun tile-windows (&optional (how :MDITILE_ZORDER) (parent *parent-window*) windows)
  (arrange-window TileWindows parent windows how (null-pointer)))

(defun cascade-windows (&optional (how :MDITILE_HORIZONTAL) (parent *parent-window*) windows)
  (arrange-window CascadeWindows parent windows how (null-pointer)))

(defun window-enabled-p (window)
  (and (window-p window)
       (IsWindowEnabled window)))

(defun window-visible-p (window)
  (and (window-p window)
       (IsWindowVisible window)))

(defun %window-focused-p (window)
  (and (window-p window)
       (pointer-eq window (GetFocus))))

(defun window-focused-p (window)
  (if (eq (GetWindowThreadId window) (GetCurrentThreadId))
      (%window-focused-p window)
      (eval-in-window window (lambda () (%window-focused-p window)))))

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
       (or (IsChild parent window)
	   (pointer-eq parent (get-parent-window window))) ; if parent is desktop window
       ))

;;; user32 - Window Control
(defun create-button (name window &key
				    (x 0)
				    (y 0)
				    (width 100)
				    (height 30)
				    (style nil)
				    (extended-style nil)
				    (default-p nil))
  (let ((button (create-window name
			       :class-name :BUTTON
			       :style (append '(:WS_TABSTOP :WS_VISIBLE :WS_CHILD :BS_PUSHBUTTON) style (when default-p '(:BS_DEFPUSHBUTTON)))
			       :extended-style extended-style
			       :parent window
			       :x x
			       :y y
			       :width width
			       :height height
			       :allow-same-name-p t)))
    (when (window-p button)
      (let ((BTNDEFPROC (make-pointer (GetWindowLongPtrW button :GWLP_WNDPROC))))
	;; Subclassing
	(message-handler+ button t (lambda (hWnd Msg wParam lParam) (CallWindowProcW BTNDEFPROC hWnd Msg wParam lParam)))
	(SetWindowLongPtrW button :GWLP_WNDPROC (pointer-address (callback MainWndProc)))
	button))))

(defun create-checkbox (name window &key
				      (x 0)
				      (y 0)
				      (width 100)
				      (height 30)
				      (style nil)
				      (extended-style nil)
				      (default-p nil))
  (declare (inline))
  (create-button name window
		 :x x
		 :y y
		 :width width
		 :height height
		 :style (append '(:BS_AUTOCHECKBOX) style)
		 :extended-style extended-style
		 :default-p default-p))

(defun create-radiobox (name window &key
				      (x 0)
				      (y 0)
				      (width 100)
				      (height 30)
				      (style nil)
				      (extended-style nil)
				      (default-p nil))
  (declare (inline))
  (create-button name window
		 :x x
		 :y y
		 :width width
		 :height height
		 :style (append '(:BS_AUTORADIOBUTTON) style)
		 :extended-style extended-style
		 :default-p default-p))

(defun create-groupbox (name window &key
				      (x 0)
				      (y 0)
				      (width 150)
				      (height 150)
				      (style nil)
				      (extended-style nil)
				      (default-p nil))
  (declare (inline))
  (create-button name window
		 :x x
		 :y y
		 :width width
		 :height height
		 :style (append '(:BS_GROUPBOX) style)
		 :extended-style extended-style
		 :default-p default-p))

;;; Editbox
(defun create-input (window &key (text "")
			      (x 0)
			      (y 0)
			      (width 150)
			      (height 30)
			      (style nil)
			      (extended-style nil))
  (let ((editor (create-window text
			       :class-name :EDIT
			       :style (append '(:WS_VISIBLE :WS_CHILD :ES_LEFT) style)
			       :extended-style extended-style
			       :parent window
			       :x x
			       :y y
			       :width width
			       :height height
			       :allow-same-name-p t)))
    (when (window-p editor)
      (let ((EDITDEFPROC (make-pointer (GetWindowLongPtrW editor :GWLP_WNDPROC))))
	;; Subclassing
	(message-handler+ editor t (lambda (hWnd Msg wParam lParam) (CallWindowProcW EDITDEFPROC hWnd Msg wParam lParam)))
	(SetWindowLongPtrW editor :GWLP_WNDPROC (pointer-address (callback MainWndProc)))
	editor))))

(defun create-editor (window &key (text "")
			       (x 0)
			       (y 0)
			       (width 400)
			       (height 300)
			       (style nil)
			       (extended-style nil))
  (declare (inline))
  (create-input window
		:text text
		:x x
		:y y
		:width width
		:height height
		:style (append '(:WS_VISIBLE :WS_CHILD :WS_VSCROLL :ES_MULTILINE :ES_AUTOVSCROLL) style)
		:extended-style extended-style))

;;; gdi32 - Drawing
(defun get-window-rectangle (window &optional client-area-p)
  (when (window-p window)
    (with-foreign-struct ((rect RECT)
			  ((:left x1))
			  ((:top  y1))
			  ((:right  x2))
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

(defun get-update-rectangle (window &optional erase-p)
  (when (window-p window)
    (with-foreign-struct ((rect RECT)
			  ((:left x1))
			  ((:top  y1))
			  ((:right  x2))
			  ((:bottom y2)))
      (GetUpdateRect window rect erase-p)
      (values x1 y1 x2 y2))))

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

(defun make-rgb-color (r g b)
  (declare (inline))
  (RGB r g b))

(defun get-color-rgb (color)
  (declare (inline))
  (values (GetRValue color)
	  (GetGValue color)
	  (GetBValue color)))

;;; helper WM_CHAR, WM_KEYDOWN, WM_KEYUP
(defun get-key-character (wParam)
  (code-char wParam))

(defun get-key-repeat-count (lParam)
  (ldb (byte 16 0) lParam))

(defun key-released-p (lParam)
  (ldb-test (byte 1 31) lParam))

(defun key-pressed-p (lParam)
  (not (key-released-p lParam)))

;;; helper for WM_XBUTTONDOWN, WM_XBUTTONUP
(defun get-cursor-x (lParam)
  (HIWORD lParam))

(defun get-cursor-y (lParam)
  (LOWORD lParam))

(defun get-modifier-key (wParam)
  (foreign-bitfield-symbols 'MK_FLAG wParam))

(defun get-key-state (key &key asynchronous)
  (if asynchronous
      (GetASyncKeyState key)
      (GetKeyState key)))

(defmacro with-drawing-object ((dc new-object &key (delete-p nil) (old-object 0)) &body body)
  (let ((old (if (symbolp old-object) old-object (gensym)))
	(new (gensym)))
    `(let* ((,new ,new-object)
	    (,old (SelectObject ,dc ,new)))
       (unwind-protect
	    (values (progn ,@body) ,(unless delete-p `,new))
	 (unless (or (null-pointer-p ,old)
		     (pointer-eq ,old (make-pointer +HGDI-ERROR+)))
	   (SelectObject ,dc ,old)
	   ,(when delete-p `(DeleteObject ,new)))))))

(defmacro with-background-mode ((dc mode) &body body)
  (let ((old (gensym)))
    `(let ((,old (SetBkMode ,dc ,mode)))
       (unwind-protect
	    (progn ,@body)
	 (unless (eq ,old :ERROR)
	   (SetBkMode ,dc ,old))))))

(defmacro with-text-metrics-info (dc (&rest slot-name-and-var-list) &body body)
  (let ((pvi (gensym)))
    `(with-foreign-struct ((,pvi TEXTMETRICW) ,@(mapcar #'list slot-name-and-var-list))
       (when (GetTextMetricsW ,dc ,pvi)
	 ,@body))))

(defun get-text-height (dc)
  (with-text-metrics-info dc ((:tmHeight height)) height))

(defun get-text-ascent (dc)
  (with-text-metrics-info dc ((:tmAscent ascent)) ascent))

(defun get-text-descent (dc)
  (with-text-metrics-info dc ((:tmDescent descent)) descent))

(defun get-text-extent (dc text)
  (with-foreign-object (size '(:struct SIZE))
    (when (GetTextExtentPoint32W dc text (length text) size)
      (values
       (foreign-slot-value size '(:struct SIZE) :cx)
       (foreign-slot-value size '(:struct SIZE) :cy)))))

(defun get-text-char-width (dc &key max-p)
  (with-text-metrics-info dc ((:tmMaxCharWidth max-width)
			      (:tmAveCharWidth ave-width))
    (if max-p max-width ave-width)))

(defun get-text-color (dc)
  (get-color-rgb (GetTextColor dc)))

(defun set-text-color (dc &key (r 0) (g 0) (b 0))
  (get-color-rgb (SetTextColor dc (make-rgb-color r g b))))

(defun get-background-color (dc)
  (get-color-rgb (GetBkColor dc)))

(defun set-background-color (dc &key (r 0) (g 0) (b 0))
  (get-color-rgb (SetBkColor dc (make-rgb-color r g b))))

(defun get-stock-object (id)
  (GetStockObject id))

(defmacro with-drawing-object-info (((object type &optional (object-info (gensym))) &rest slot-name-and-var-list) &body body)
  (let* ((size (gensym)))
    `(let ((,size (GetObjectW ,object 0 (null-pointer))))
       (when (> ,size 0)
	 (with-foreign-struct ((,object-info ,type) ,@(mapcar #'list slot-name-and-var-list))
	   (let ((,size (GetObjectW ,object ,size ,object-info)))
	     (when (> ,size 0)
	       ,@body)))))))

(defun create-font (&key
		      (height 0)
		      (width 0)
		      (escapement 0)
		      (orientation 0)
		      (weight 0)
		      italic
		      underline
		      strikeout
		      (charset :DEFAULT_CHARSET)
		      (precision '(:OUT_DEFAULT_PRECIS :CLIP_DEFAULT_PRECIS))
		      (quality :DEFAULT_QUALITY)
		      (pitch :DEFAULT_PITCH)
		      (family :DONTCARE)
		      (face ""))
  (CreateFontW height
	       width
	       (ceiling (* escapement 10))
	       (ceiling (* orientation 10))
	       (if (numberp weight) weight (foreign-enum-value 'FW_ENUM weight))
	       italic
	       underline
	       strikeout
	       charset
	       (first precision)
	       (second precision)
	       quality
	       (logior (foreign-enum-value 'PITCH_ENUM pitch) (ash (foreign-enum-value 'FAMILY_ENUM family) 4))
	       face))

(defun destroy-font (font)
  (unless (null-pointer-p font)
    (DeleteObject font)))

(defun get-current-font (dc)
  (GetCurrentObject dc :OBJ_FONT))

(defmacro parse-font-info (font-info)
  `(parse-foreign-struct ((,font-info LOGFONTW)
			  ((:lfHeight height))
			  ((:lfWidth width))
			  ((:lfEscapement escapement))
			  ((:lfOrientation orientation))
			  ((:lfWeight weight)) ; FW_ENUM can be used
			  ((:lfItalic italic))
			  ((:lfUnderline underline))
			  ((:lfStrikeOut strikeout))
			  ((:lfCharSet charset)) ;CHARSET_ENUM
			  ((:lfOutPrecision out-precision)) ;OUT_PRECIS_ENUM
			  ((:lfClipPrecision clip-precision)) ;CLIP_PRECIS_ENUM
			  ((:lfQuality quality)) ;QUALITY_ENUM
			  ((:lfPitchAndFamily pitch&family)) ;PITCH_ENUM FAMILY_ENUM
			  ((:lfFaceName face)))
     (list :height height
	   :width width
	   :escapement (/ escapement 10.0)
	   :orientation (/ orientation 10.0)
	   :weight (or (foreign-enum-keyword 'FW_ENUM weight :errorp nil) weight)
	   :italic (/= italic 0)
	   :underline (/= underline 0)
	   :strikeout (/= strikeout 0)
	   :charset (foreign-enum-keyword 'CHARSET_ENUM charset)
	   :precision (list (foreign-enum-keyword 'OUT_PRECIS_ENUM out-precision)
			    (foreign-enum-keyword 'CLIP_PRECIS_ENUM clip-precision))
	   :quality (foreign-enum-keyword 'QUALITY_ENUM quality)
	   :pitch (foreign-enum-keyword 'PITCH_ENUM (ldb (byte 4 0) pitch&family))
	   :family (foreign-enum-keyword 'FAMILY_ENUM (ldb (byte 4 4) pitch&family))
	   :face (foreign-string-to-lisp face :max-chars +LF_FACESIZE+))))

(defun get-font-info (font)
  (with-drawing-object-info ((font LOGFONTW font-info))
    (parse-font-info font-info)))

(defcallback EnumFontFamExCallback :boolean
    ((lpelfe (:pointer (:struct LOGFONTW)))
     (lpntme (:pointer (:struct TEXTMETRICW)))
     (FontType DWORD)
     (lParam LPARAM))
  (declare (special font-infos) (ignore lpntme FontType lParam))
  (setf font-infos (cons (parse-font-info lpelfe) font-infos)))
(defun get-all-font-infos (dc &key (charset :DEFAULT_CHARSET) (face ""))
  (let ((font-infos nil))
    (declare (special font-infos))
    (with-foreign-struct ((font LOGFONTW)
			  (:lfCharSet (foreign-enum-value 'CHARSET_ENUM charset))
			  ((:lfFaceName lfFaceName)))
      (cffi:lisp-string-to-foreign face lfFaceName +LF_FACESIZE+)
      (EnumFontFamiliesExW dc font (callback EnumFontFamExCallback) 0 0)) 
    font-infos))

(defun create-pen (&key
		     (type :PS_GEOMETRIC)
		     (style :PS_SOLID)
		     (endcap :PS_ENDCAP_FLAT)
		     (join :PS_JOIN_ROUND)
		     (width 1)
		     (brush :BS_SOLID)
		     (color '(0 0 0))
		     (hatch 0))
  (let ((color (apply #'make-rgb-color color)))
    (cond ((and style (listp style))
	   (with-foreign-struct ((brush-info LOGBRUSH)
				 ((:lbStyle) brush)
				 ((:lbColor) color)
				 ((:lbHatch) (if (numberp hatch) hatch (foreign-enum-value 'HS_ENUM hatch))))
	     (with-foreign-object (style-array 'DWORD (length style))
	       (loop for s in style and i from 0 do (setf (mem-aref style-array 'DWORD i) s))
	       (ExtCreatePen (logior (foreign-enum-value 'PS_STYLE_ENUM :PS_USERSTYLE)
				     (foreign-enum-value 'PS_ENDCAP_ENUM endcap)
				     (foreign-enum-value 'PS_JOIN_ENUM join)
				     (foreign-enum-value 'PS_TYPE_ENUM type))
			     width
			     brush-info
			     (length style)
			     style-array))))
	  (brush (with-foreign-struct ((brush-info LOGBRUSH)
				       ((:lbStyle) brush)
				       ((:lbColor) color)
				       ((:lbHatch) (if (numberp hatch) hatch (foreign-enum-value 'HS_ENUM hatch))))
		   (ExtCreatePen (logior (foreign-enum-value 'PS_STYLE_ENUM style)
					 (foreign-enum-value 'PS_ENDCAP_ENUM endcap)
					 (foreign-enum-value 'PS_JOIN_ENUM join)
					 (foreign-enum-value 'PS_TYPE_ENUM type))
				 width
				 brush-info
				 0
				 (null-pointer))))
	  (t (CreatePen style width color)))))

(defun destroy-pen (pen)
  (unless (null-pointer-p pen)
    (DeleteObject pen)))

(defmacro parse-pen-info (pen-info)
  `(parse-foreign-struct ((,pen-info EXTLOGPEN)
			  ((:elpPenStyle type&style&endcap&join)) ;PS_ENUM, PS_FLAG
			  ((:elpWidth width))
			  ((:elpBrushStyle brush))
			  ((:elpColor  color))
			  ((:elpHatch hatch)) ;HS_ENUM or pointer to bitmap
			  ((:elpNumEntries style-length)))
     (let ((type   (foreign-enum-keyword 'PS_TYPE_ENUM   (logand +PS_TYPE_MASK+   type&style&endcap&join)))
	   (style  (foreign-enum-keyword 'PS_STYLE_ENUM  (logand +PS_STYLE_MASK+  type&style&endcap&join)))
	   (endcap (foreign-enum-keyword 'PS_ENDCAP_ENUM (logand +PS_ENDCAP_MASK+ type&style&endcap&join)))
	   (join   (foreign-enum-keyword 'PS_JOIN_ENUM   (logand +PS_JOIN_MASK+   type&style&endcap&join)))
	   (custom (loop for i from 0 below style-length collect
			(mem-aref (foreign-slot-pointer ,pen-info '(:struct EXTLOGPEN) :elpStyleEntry) 'DWORD i)))
	   (color  (multiple-value-list (get-color-rgb color))))
       (list :type type
	       :style (if (eq style :PS_USERSTYLE) custom style)
	       :endcap endcap
	       :join   join
	       :width width
	       :brush brush
	       :color color
	       :hatch (or (foreign-enum-keyword 'HS_ENUM hatch :errorp nil) hatch)))))

(defun get-pen-info (pen)
  (with-drawing-object-info ((pen EXTLOGPEN pen-info))
    (parse-pen-info pen-info)))

(defun create-brush (&key
		       ((:style s) :BS_SOLID)
		       ((:color c) (list 0 0 0))
		       ((:hatch h) :HS_VERTICAL))
  (with-foreign-struct ((brush LOGBRUSH)
			((:lbStyle style) s)
			((:lbColor color) (apply #'make-rgb-color c))
			((:lbHatch hatch) (if (numberp h) h (foreign-enum-value 'HS_ENUM h :errorp nil))))
    (CreateBrushIndirect brush)))

(defun destroy-brush (brush)
  (unless (null-pointer-p brush)
    (DeleteObject brush)))

(defmacro parse-brush-info (brush-info)
  `(parse-foreign-struct ((,brush-info LOGBRUSH)
			  ((:lbStyle style))
			  ((:lbColor color))
			  ((:lbHatch hatch)))
     
     (list :style style
	   :color (multiple-value-list (get-color-rgb color))
	   :hatch (or (foreign-enum-keyword 'HS_ENUM hatch :errorp nil) hatch))))

(defun get-brush-info (brush)
  (with-drawing-object-info ((brush LOGBRUSH brush-info))
    (parse-brush-info brush-info)))
