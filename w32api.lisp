(in-package #:w32api)

;;; "w32api" goes here. Hacks and glory await!

;;; kernel32
(defun get-error ()
  (GetLastError))

(defun print-error (error-code)
  (let ((dwFLAGS (logior  #X00001000 #X00000200 #X00000100)))
    (with-foreign-object (strptr :pointer)
      (FormatMessageW dwFLAGS (null-pointer) error-code 0 strptr 0 (null-pointer))
      (foreign-string-to-lisp (mem-ref strptr :pointer)))))

;;; user32
(defun create-desktop (name)
  (when (stringp name)
    (with-foreign-object (sa '(:struct SECURITY_ATTRIBUTES))

      (setf (foreign-slot-value sa '(:struct SECURITY_ATTRIBUTES) :nLength) (foreign-type-size '(:struct SECURITY_ATTRIBUTES)))
      (setf (foreign-slot-value sa '(:struct SECURITY_ATTRIBUTES) :lpSecurityDescriptor) (null-pointer))
      (setf (foreign-slot-value sa '(:struct SECURITY_ATTRIBUTES) :bInheritHandle) t)

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

(defun get-all-desktop ()
  (let ((winsta (GetProcessWindowStation))
	(desktops nil))
    (unless (null-pointer-p winsta)
      (with-callback (collect :boolean ((lpszDesktop :string) (lParam LPARAM))
			      (declare (ignore lParam))
			      (setf desktops (cons lpszDesktop desktops)))
	(EnumDesktopsW winsta collect 0)))
    desktops))

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

(defvar *create-window-owned-classes* (make-hash-table))
(defvar *create-window-owned-procedures* (make-hash-table))
(defvar *create-window-lock* (make-recursive-lock))

(defvar *parent-window* (null-pointer))
(defvar *parent-window-lock* (make-recursive-lock))

(defmacro with-parent-window ((parent) &body body)
  (let ((p (gensym)))
    `(with-recursive-lock-held (*parent-window-handle-lock*)
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

(defmacro with-class ((name &rest args) &body body)
  `(unless (eq (register-class ,name ,@args) 0)
     (unwind-protect
	  (progn ,@body)
       (unregister-class ,name))))

(defcallback MainWndProc LRESULT
    ((hWnd   HWND)
     (Msg    WND_MESSAGE)
     (wParam WPARAM)
     (lParam LPARAM))
  (with-recursive-lock-held (*create-window-lock*)
    (let* ((cont (lambda (hWnd Msg wParam lParam cont)
		   (declare (ignore cont))
		   (case Msg
		     (:WM_DESTROY (post-quit-message 0))
		     (:WM_CLOSE   (destroy-window hWnd))
		     (t (DefWindowProcW hWnd Msg wParam lParam)))))
	   (proc (gethash (pointer-address hWnd) *create-window-owned-procedures* cont)))
      (let ((result (funcall proc hWnd Msg wParam lParam (lambda () (funcall cont hWnd Msg wParam lParam nil)))))
	(if (numberp result) result 0)))))

(defun register-class (name
		       &key
			 (procedure (callback MainWndProc))
			 (style '(:CS_HREDRAW :CS_VREDRAW)))
  (if (stringp name)
      (with-foreign-object (wnd-class '(:struct WNDCLASSEX))
	
	(setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) :cbSize) (foreign-type-size '(:struct WNDCLASSEX)))
	(setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) :style) style)
	(setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) :lpfnWndProc) procedure)
	(setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) :cbClsExtra) 0)
	(setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) :cbWndExtra) 0)
	(setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) :hInstance) (GetModuleHandleW (null-pointer)))
	(setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) :hIcon) (null-pointer))
	(setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) :hCursor) (null-pointer))
	(setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) :hbrBackground) (null-pointer))
	(setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) :lpszMenuName) (null-pointer))
	(setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) :lpszClassName) name)
	(setf (foreign-slot-value wnd-class '(:struct WNDCLASSEX) :hIconSm) (null-pointer))

	(RegisterClassExW wnd-class))
      0))

(defun unregister-class (name)
  (when (stringp name)
    (UnregisterClassW name (GetModuleHandleW (null-pointer)))))

(defun create-window (name
		      &key
			(class-name name)
			(procedure nil procedure-p)
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
      (with-recursive-lock-held (*create-window-lock*)
	(unless (eq atom 0)
	  (if (null-pointer-p hWnd)
	      (unregister-class class-name)
	      (setf (gethash (pointer-address hWnd) *create-window-owned-classes*) class-name)))
	(unless (null-pointer-p hWnd)
	  (when procedure-p
	    (setf (gethash (pointer-address hWnd) *create-window-owned-procedures*) procedure))
	  hWnd)))))

(defun set-window-procedure (window procedure)
  (when (and (window-p window) (functionp procedure))
    (with-recursive-lock-held (*create-window-lock*)
      (setf (gethash (pointer-address window) *create-window-owned-procedures*) procedure)
      t)))

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

(defun get-descendant-windows (window)
  (when (window-p window)
    (let ((descendant nil))
      (with-callback
	  (collect :boolean ((window HWND) (lparam LPARAM))
		   (declare (ignore lparam))
		   (setf descendant (cons window descendant)))
	(EnumChildWindows window collect 0))
      descendant)))

(defun get-desktop-window ()
  (let ((window (GetDesktopWindow)))
    (unless (null-pointer-p window) window)))

(defun get-window-class-name (window)
  (when (window-p window)
    (string-trim " " (with-foreign-pointer-as-string
			 ((class-name class-name-length) 1024)
		       (GetClassNameW window class-name class-name-length)))))

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
      (with-recursive-lock-held (*create-window-lock*)
	(remhash (pointer-address window) *create-window-owned-procedures*)
	(let ((class-name (gethash (pointer-address window) *create-window-owned-classes*)))
	  (when class-name
	    (unregister-class class-name)
	    (remhash (pointer-address window) *create-window-owned-classes*)))))))

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

(defun update-window (window)
  (when (window-p window)
    (UpdateWindow window)))

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

(defun start-window (&rest args)
  (make-thread
   (lambda ()
     (let ((hWnd (apply #'create-window args)))
       (when hWnd
	 (unwind-protect
	      (progn (show-window hWnd)
		     (process-message))
	   (destroy-window hWnd)))))))

;;; Button

(defun create-button (name window &key
				    (x 0)
				    (y 0)
				    (width 100)
				    (height 30)
				    (style nil)
				    (on-click nil)
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
    (set-window-procedure
     button
     (lambda (hWnd Msg wParam lParam cont)
       (declare (ignore cont))
       (print Msg)
       (case Msg
       	 (:WM_LBUTTONDOWN (and (functionp on-click) (funcall on-click)))
	 (t))
       (CallWindowProcW BTNDEFPROC hWnd Msg wParam lParam)))
    
    (SetWindowLongPtrW button :GWLP_WNDPROC (pointer-address (callback MainWndProc)))
    button))

(defun create-checkbox (name window &key
				      (x 0)
				      (y 0)
				      (width 100)
				      (height 30)
				      (style nil)
				      (on-check nil)
				      (default-p nil))
  (declare (inline))
  (create-button name window
		 :x x
		 :y y
		 :width width
		 :height height
		 :style (append '(:BS_AUTOCHECKBOX) style)
		 :on-click on-check
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
    (set-window-procedure
     editor
     (lambda (hWnd Msg wParam lParam cont)
       (declare (ignore cont))
       (case Msg
	 (t))
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

;;; DC and Drawing
(defun get-window-rectangle (window &optional client-area-p)
  (when (window-p window)
    (with-foreign-object (rect '(:struct RECT))
      (if client-area-p
	  (GetClientRect window rect)
	  (GetWindowRect window rect))
      (values
       (foreign-slot-value rect '(:struct RECT) :left)
       (foreign-slot-value rect '(:struct RECT) :top)
       (foreign-slot-value rect '(:struct RECT) :right)
       (foreign-slot-value rect '(:struct RECT) :bottom)))))

(defun get-window-size (window &optional client-area-p)
  (when (window-p window)
    (multiple-value-bind (left top right bottom)
	(get-window-rectangle window client-area-p)
      (values (- right left)
	      (- bottom top)))))

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
