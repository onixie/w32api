(in-package #:w32api/test)

(def-suite test/w32api :in test)

(in-suite test/w32api)

;;;; kernel32 api
(test |(get-processor-type) return keyword for processor type|
  (is (keywordp (get-processor-type))))

(test |(get-processor-arch) return keyword for processor arch|
  (is (keywordp (get-processor-arch))))

(test |(get-processor-arch) confirms (get-processor-type)| ;checkme: loosely checked
  (when (eq (get-processor-arch) :PROCESSOR_ARCHITECTURE_AMD64)
    (is (eq (get-processor-type) :PROCESSOR_AMD_X8664)))
  (when (or (eq (get-processor-arch) :PROCESSOR_ARCHITECTURE_IA64)
	    (eq (get-processor-arch) :PROCESSOR_ARCHITECTURE_IA32_ON_WIN64)
	    (eq (get-processor-arch) :PROCESSOR_ARCHITECTURE_INTEL))
    (is (member (get-processor-type) '(:PROCESSOR_INTEL_386
				       :PROCESSOR_INTEL_486
				       :PROCESSOR_INTEL_PENTIUM
				       :PROCESSOR_INTEL_IA64))))
  (when (or (eq (get-processor-arch) :PROCESSOR_ARCHITECTURE_ARM)
	    (eq (get-processor-arch) :PROCESSOR_ARCHITECTURE_ARM64))
    (is (member (get-processor-type) '(:PROCESSOR_STRONGARM
				       :PROCESSOR_ARM720
				       :PROCESSOR_ARM820
				       :PROCESSOR_ARM920
				       :PROCESSOR_ARM_7TDMI))))
  (skip (format nil "Processor Type: ~s, Processor Arch: ~s"
		(get-processor-type)
		(get-processor-arch))))

(test |(get-processor-count) return number of logical processors|
  (is (numberp (get-processor-count)))
  (skip (format nil "The Number Of Processors: ~d" (get-processor-count))))

(test |(processor-feature-present-p :PF_SSE3_INSTRUCTIONS_AVAILABLE) return t if support SSE3|
  (skip (format nil "Does processor support SSE3: ~a" (processor-feature-present-p :PF_SSE3_INSTRUCTIONS_AVAILABLE))))

(test |(get-firmware-type) return keyword for firmware type|
  (is (keywordp (get-firmware-type)))
  (skip (format nil "Firmware Type: ~a" (get-firmware-type))))

(test |(boot-from-vhd-p) return t if boot from vhd|
  (skip (format nil "Is booted from VHD: ~a" (boot-from-vhd-p))))

(test |(get-product-type) return keyword for product type|
  (is (keywordp (get-product-type)))
  (skip (format nil "Product Type: ~a" (get-product-type))))

(test |(get-computer-name) return current computer name|
  (is (stringp (get-computer-name)))
  (skip (format nil "Computer Name: ~a" (get-computer-name))))

(test |(get-user-name) return current user name|
  (is (stringp (get-user-name)))
  (skip (format nil "User Name: ~a" (get-user-name))))

(test |(get-windows-directory) return current windows directory|
  (is (stringp (get-windows-directory)))
  (skip (format nil "Windows Directory: ~a" (get-windows-directory))))

(test |(get-windows-directory :system-p t) return current system windows directory|
  (is (stringp (get-windows-directory :system-p t)))
  (skip (format nil "System Windows Directory: ~a" (get-windows-directory :system-p t))))

(test |(get-system-directory) return current system directory|
  (is (stringp (get-system-directory)))
  (skip (format nil "System Directory: ~a" (get-system-directory))))

(test |(get-os-version) = (values major minor sp-major sp-minor)|
  (multiple-value-bind (major minor sp-major sp-minor)
      (get-os-version)
    (is (<= 5 major 10))
    (skip (format nil "OS Version: ~d.~d, SP Version: ~d.~d" major minor sp-major sp-minor))))

(test |(get-os-buile-number) return build numbers|
  (skip (format nil "OS Build Number: ~d" (get-os-build-number))))

(test |(get-error) = 0 if no errors|
  (is (equal 0 (get-error))))

(test |(format-error 0) = string indicates no errors|
  (is (string-equal "The operation completed successfully."
		    (format-error 0 :lang :LANG_ENGLISH :sublang :SUBLANG_ENGLISH_US))))

(test |(print-error 0) = (values code message)|
  (multiple-value-bind (code message)
      (print-error :lang :LANG_ENGLISH :sublang :SUBLANG_ENGLISH_US)
    ;; I guess there are no errors before here
    (is (eq 0 code))
    (is (string-equal "The operation completed successfully." message))))

;;; user32 api
(def-fixture class-name (<class-name>)
  (print <class-name>)
  (&body))

(def-fixture window-name (<window-name>)
  (print <window-name>)
  (&body))

(def-fixture class (<class-name>)
  (print <class-name>)
  (with-class (<class-name> (callback w32api::MainWndProc))
    (&body)))

(def-fixture window (<window-name>
		     &key
		     ((:class-name <class-name>) <window-name>)
		     ((:parent <parent-window>) (null-pointer))
		     ((:owner <owner-window>) (null-pointer)))
  (print <window-name>)
  (with-window (<window> <window-name> :class-name <class-name> :parent <parent-window> :owner <owner-window>)
    (&body)))

(test |(get-all-monitors) = all monitors|
  (is (get-all-monitors))		;this is a gui wrapper, so monitor is necessity i guess
  (is-false (some #'null-pointer-p (get-all-monitors))))

(test |(get-monitor) = nearest monitor from (0, 0)|
  (is (get-monitor)))

(test |(get-monitor :x1 x :y1 y :x2 x2 :y2 y2) = intersected monitor|
  (is (get-monitor :x1 0 :y1 0 :x2 1 :y2 1)))

(test |(get-window-monitor <window>) = intersected monitor|
  (is (get-window-monitor (get-desktop-window))))

(test |(get-monitor-name <monitor>) = name of monitor|
  (is (stringp (get-monitor-name (get-window-monitor (get-desktop-window))))))

(test |(get-monitor-rectangle <monitor>) = monitor full area|
  (multiple-value-bind (left top right bottom)
      (get-monitor-rectangle (get-window-monitor (get-desktop-window)))
    (is (numberp left))
    (is (numberp top))
    (is (numberp right))
    (is (numberp bottom))))

(test |(get-monitor-rectangle <monitor> :rcWork) = monitor working area|
  (multiple-value-bind (left top right bottom)
      (get-monitor-rectangle (get-window-monitor (get-desktop-window)) :rcWork)
    (is (numberp left))
    (is (numberp top))
    (is (numberp right))
    (is (numberp bottom))))

(test |(get-current-desktop) return current desktop|
  (is (not (null-pointer-p (get-current-desktop)))))

(test |(get-window-desktop <window>) return desktop in which the window is|
  (with-fixture window ((string (gensym "WIN")))
    (is (pointer-eq  (get-current-desktop) (get-window-desktop <window>)))))

(test |(create-desktop <name>) return new created desktop|
  (let ((desk (create-desktop (string (gensym "DESK")))))
    (is (not (null-pointer-p desk)))
    (is-true (destroy-desktop desk))))

(test |(desktop-p <desktop>) = <desktop>|
  (is (pointer-eq (desktop-p (get-current-desktop)) (get-current-desktop))))

(test |(get-desktop-name <desktop>) = string to indicate desktop name|
  (is (stringp (get-desktop-name (get-current-desktop)))))

(test |(create-desktop <non-string>) = nil|
  (is (eq nil (create-desktop 'non-string))))

(test |(switch-desktop <desktop>) = t will switch current desktop|
  (let ((old (get-current-desktop))
	(new1 (create-desktop (string (gensym "DESK"))))
	(new2 (create-desktop (string (gensym "DESK")))))

    (is-true (switch-desktop new1))
    (is (pointer-eq new1 (get-current-desktop)))
    (is-true (switch-desktop new2))    
    (is (pointer-eq new2 (get-current-desktop)))
    (is-true (switch-desktop old))    
    (is (pointer-eq old (get-current-desktop)))
    
    (destroy-desktop new1)
    (destroy-desktop new2)))

(test |(switch-desktop ... (switch-desktop <desktop>)) chains will not hold <desktop>|
  (let ((desktop-name (string (gensym "DESK"))))
    (is (not (member desktop-name (get-all-desktops) :test #'string-equal)))
    (destroy-desktop
     (switch-desktop
      (switch-desktop
       (switch-desktop
	(switch-desktop
	 (create-desktop desktop-name))))))
    (is (not (member desktop-name (get-all-desktops) :test #'string-equal)))))

(test |call on <invalid-desktop/monitor> should return nil| 
  (dolist (func (list
		 #'get-monitor-name
		 #'get-monitor-rectangle
		 #'get-desktop-name
		 #'desktop-p
		 #'switch-desktop
		 #'destroy-desktop
		 ))
    (is (equal nil (funcall func (null-pointer))))
    (is (equal nil (funcall func (make-pointer #x1))))
    (with-foreign-object (invalid :int)
      (is (equal nil (funcall func invalid))))))

(test |(register-class <new-name>) /= 0 to indicate no errors|
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (is (not (equal 0 (register-class <class-name> (callback w32api::MainWndProc)))))
    (unregister-class <class-name>)))

(test |(register-class <exist-name>) = 0 to indicate error|
  (with-fixture class ((string (gensym "WINCLASS")))
    (is (equal 0 (register-class <class-name> (callback w32api::MainWndProc))))))

(test |(unregister-class <new-name>) = nil| 
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (is (equal nil (unregister-class <class-name>)))))

(test |(unregister-class <exist-name>) = t| 
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (register-class <class-name> (callback w32api::MainWndProc))
    (is (equal t (unregister-class <class-name>)))))

(test |(w32api::%create-window <new-name>) = <window>| 
  (with-fixture window-name ((string (gensym "WIN")))
    (let ((<window> (w32api::%create-window <window-name>)))
      (is-true (window-p <window>))
      (destroy-window <window>))))

(test |(w32api::%create-window <new-name> :class-name <new-name>) = <window>| 
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (with-fixture window-name ((string (gensym "WIN")))
      (let ((<window> (w32api::%create-window <window-name> :class-name <class-name>)))
	(is-true (window-p <window>))
	(is (equal <class-name> (get-window-class-name <window>)))
	(destroy-window <window>)))))

(test |(w32api::%create-window <new-name> :class-name <exist-name>) = <window>| 
  (with-fixture class ((string (gensym "WINCLASS")))
    (with-fixture window-name ((string (gensym "WIN")))
      (let ((<window> (w32api::%create-window <window-name> :class-name <class-name>)))
	(is-true (window-p <window>))
	(is (equal <class-name> (get-window-class-name <window>)))
	(destroy-window <window>)))))

(test |(w32api::%create-window <new-name> :parent <exist-window>) = <window>| 
  (with-fixture window ((string (gensym "WIN")))
    (with-fixture window-name ((string (gensym "WIN")))
      (let ((<child-window> (w32api::%create-window <window-name> :parent <window>)))
	(is-true (window-p <child-window>))
	(is-true (parent-window-p <child-window> <window>))
	(destroy-window <child-window>)))))

(test |(w32api::%create-window <exist-name>) = nil| 
  (with-fixture window ((string (gensym "WIN")))
    (is-false (w32api::%create-window <window-name>))))

(test |(create-window <exist-name>) = <window> if <exist-name> is in another thread| 
  (with-fixture window ((string (gensym "WIN")))
    (is (destroy-window (window-p (create-window <window-name>))))))

(test |(create-window <new-name> :desktop <desktop-name>) will create window in new desktop|
  (let* ((d (create-desktop (string (gensym "DESK"))))
	 (w (create-window (string (gensym "WIN")) :desktop d)))
    (is (not (pointer-eq (get-desktop-window) (get-parent-window w))))
    (is (destroy-window w))))

(test |(get-window <window-name>) = <window>|
  (with-fixture window ((string (gensym "WIN")))
    (is (window-p (get-window <window-name>)))))

(test |(get-window <window-name> :class-name <class-name>) = <window>|
  (with-fixture window ((string (gensym "WIN")))
    (is (window-p (get-window <window-name> :class-name <window-name>)))))

(test |(get-window <window-name> :parent <parent-window>) = <window>|
  (with-fixture window ((string (gensym "WIN")))
    (let ((<parent-window> <window>))
      (with-fixture window ((string (gensym "WIN")) :parent <parent-window>)
	(is (window-p <parent-window>))
	(is (window-p <window>))
	(is (parent-window-p <window> <parent-window>))
	(is (eq nil (find-window <window-name>)))
	(is (pointer-eq <window> (find-window <window-name> :parent <parent-window>)))
	(is (pointer-eq <window> (get-window <window-name>)))
	(is (pointer-eq <window> (get-window <window-name> :current-thread-window-p nil)))
	(is (pointer-eq <window> (get-window <window-name> :parent <parent-window>)))
	))))

(test |call on <invalid-window> should return nil| 
  (dolist (func (list
		 #'get-window-desktop
		 #'window-p
		 #'get-owner-window
		 #'get-parent-window
		 (lambda (window)
		   (with-fixture window ((string (gensym "PWIN")))
		     (set-parent-window window <window>)))
		 (lambda (window)
		   (get-ancestor-window window :ga_parent))
		 #'get-child-window
		 #'get-children-windows
		 #'get-descendant-windows
		 #'get-window-class-name
		 #'destroy-window
		 #'foreground-window
		 #'show-window
		 #'hide-window
		 #'enable-window
		 #'disable-window
		 #'active-window
		 #'switch-window
		 #'focus-window
		 #'raise-window
		 #'bury-window
		 (lambda (window)
		   (move-window window 0 0))
		 (lambda (window)
		   (resize-window window 100 100))
		 (lambda (window)
		   (move-&-resize-window window 0 0 100 100))
		 (lambda (window)
		   (set-window-title window "Title"))
		 #'get-window-title
		 #'get-window-style
		 (lambda (invalid-window)
		   (set-window-style invalid-window))
		 #'maximize-window
		 #'minimize-window
		 #'restore-window
		 (lambda (invalid-window)
		   (with-window (valid-window (string (gensym "WIN")))
		     (and
		      (parent-window-p valid-window invalid-window)
		      (parent-window-p invalid-window valid-window))))
		 #'window-visible-p
		 #'window-enabled-p
		 #'window-active-p
		 #'window-foregrounded-p
		 #'window-focused-p
		 #'window-minimized-p
		 #'window-maximized-p
		 ))
    (is (equal nil (funcall func (null-pointer))))
    (is (equal nil (funcall func (make-pointer #x1))))
    (with-foreign-object (invalid :int)
      (is (equal nil (funcall func invalid)))))
  )

(test |(get-window-style <window>) = (list <style> ...)|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal (list :WS_OVERLAPPED) (set-difference w32api.type:+WS_OVERLAPPEDWINDOW+ (get-window-style <window>)))))
  (with-fixture window ((string (gensym "WIN")))
    (set-window-style <window> :WS_OVERLAPPED)
    (is (equal (list :WS_CLIPSIBLINGS) (get-window-style <window>))))
  (with-fixture window ((string (gensym "WIN")))
    (set-window-style <window> :WS_CHILD)
    (is (member :WS_CHILD (get-window-style <window>))))
  )

(test |(set-window-style <window>) will set default style to <window>|
  (with-fixture window ((string (gensym "WIN")))
    (set-window-style <window> :WS_CHILD)
    (set-window-style <window>)
    (is (not (member :WS_CHILD (get-window-style <window>))))
    (is (equal (list :WS_OVERLAPPED) (set-difference w32api.type:+WS_OVERLAPPEDWINDOW+ (get-window-style <window>)))))
  )

(test |(set-window-style <window> <style>) will set <style> to <window>|
  (with-fixture window ((string (gensym "WIN")))
    (set-window-style <window> :WS_CHILD)
    (is (member :WS_CHILD (get-window-style <window>)))
    (set-window-style <window>)		;fixme : :WS_CHILD will make switch-desktop fail
    )
  )

(test |(set-window-style <window> <style>) will set <style> to <window>|
  (with-fixture window ((string (gensym "WIN")))
    (set-window-style <window> (list :WS_GROUP :BS_CHECKBOX))
    (is (member :WS_GROUP (get-window-style <window>)))
    (is (member :BS_CHECKBOX (get-window-style <window>))))
  )

(test |(get-owner-window <window>) will return owner of <window>|
  (with-fixture window ((string (gensym "WIN")))
    (with-fixture window ((string (gensym "WIN")) :owner <window>)
      (is (pointer-eq <owner-window> (get-owner-window <window>)))
      (is (pointer-eq (get-desktop-window) (get-parent-window <window>))))))

(test |(get-parent-window <window>) will return parent of <window>|
  (with-fixture window ((string (gensym "WIN")))
    (with-fixture window ((string (gensym "WIN")) :parent <window>)
      (is (pointer-eq <parent-window> (get-parent-window <window>)))
      (is-false (get-owner-window <window>)))))

(test |(get-parent-window <pop-up-window>) will return desktop window as parent|
  (with-fixture window ((string (gensym "WIN")))
    (is (pointer-eq (get-desktop-window) (get-parent-window <window>)))
    (is (parent-window-p <window> (get-desktop-window)))
    (is (member <window> (get-children-windows (get-desktop-window)) :test #'pointer-eq))
    (is (member <window> (get-descendant-windows (get-desktop-window)) :test #'pointer-eq))))

(test |(set-parent-window <window> <parent>) will set <parent> as parent of <window>|
  (with-fixture window ((string (gensym "WIN")))
    (let ((<parent> <window>))		;Notice <parent-window> is a dynamic binding in with-fixture
      (with-fixture window ((string (gensym "WIN")))
	(is (not (parent-window-p <window> <parent>)))
	(set-parent-window <window> <parent>)
	(is (parent-window-p <window> <parent>))
	(set-parent-window <window>)
	(is (not (parent-window-p <window> <parent>)))))))

(test |(get-ancestor-window <window> :parent) = (get-parent-window <window>|
  (with-fixture window ((string (gensym "WIN")))
    (with-fixture window ((string (gensym "WIN")) :parent <window>)
      (is (pointer-eq <parent-window> (get-ancestor-window <window> :ga_parent))))))

(test |(get-ancestor-window <window> :root) = (get-parent-window ...(get-parent-window <window>))|
  (with-fixture window ((string (gensym "WIN")))
    (with-fixture window ((string (gensym "WIN")) :parent <window>)
      (with-fixture window ((string (gensym "WIN")) :parent <window>)
	(is (pointer-eq (get-parent-window <parent-window>) (get-ancestor-window <window> :ga_root)))))))

(test |(get-desktop-window) will return desktop window in current screen|
  (is (not (null-pointer-p (get-desktop-window)))))

(test |(get-child-window <window>) will return first child, :reverse t will return last child|
  (with-fixture window ((string (gensym "WIN")))
    (let ((children (loop repeat 10 collect (w32api::%create-window (string (gensym "WIN")) :parent <window>))))
      (is (pointer-eq (first children) (get-child-window <window>)))
      (is (pointer-eq (first (last children)) (get-child-window <window> :reverse t)))
      (mapc #'destroy-window children))))

(test |(get-child-window <window> :nth <n>) will return nth child if n < child count|
  (with-fixture window ((string (gensym "WIN")))
    (let ((children (loop repeat 10 collect (w32api::%create-window (string (gensym "WIN")) :parent <window>))))
      (is (pointer-eq (third children) (get-child-window <window> :nth 3)))
      (is (pointer-eq (third children) (get-child-window <window> :nth 8 :reverse t)))
      (mapc #'destroy-window children))))

(test |(get-child-window <window> :nth <n>) will return nil if n > child count or < 1|
  (with-fixture window ((string (gensym "WIN")))
    (is (eq nil (get-child-window <window>)))
    (let ((children (loop repeat 10 collect (w32api::%create-window (string (gensym "WIN")) :parent <window>))))
      (is (eq nil (get-child-window <window> :nth 11)))
      (is (eq nil (get-child-window <window> :nth 11 :reverse t)))
      (is (eq nil (get-child-window <window> :nth 0)))
      (is (eq nil (get-child-window <window> :nth 0 :reverse t)))
      (mapc #'destroy-window children))))

(test |(get-children-windows <window>) will return all children windows|
  (with-fixture window ((string (gensym "WIN")))
    (is (eq nil (get-children-windows <window>)))
    (let ((children (loop repeat 10 collect (w32api::%create-window (string (gensym "WIN")) :parent <window>))))
      (is (eq 10 (length (get-children-windows <window>))))
      (is (eq nil (set-exclusive-or
		   children
		   (get-children-windows <window>)
		   :key #'pointer-address)))
      (mapc #'destroy-window children)
      )))

(test |(get-descendant-windows <window>) will return all descendants windows|
  (with-fixture window ((string (gensym "WIN")))
    (is (eq nil (get-descendant-windows <window>)))
    (with-fixture window ((string (gensym "WIN")) :parent <window>)
      (is (member <window> (get-descendant-windows <parent-window>)
		  :test #'pointer-eq))
      (let ((children (loop repeat 10 collect (w32api::%create-window (string (gensym "WIN")) :parent <window>))))
	(is (eq 11 (length (get-descendant-windows <parent-window>))))
	(is (member <window>
		    (set-difference
		     (get-descendant-windows <parent-window>)
		     children
		     :key #'pointer-address)
		    :test #'pointer-eq))
	(mapc #'destroy-window children)
	))))

(test |(window-p <window>) = <window>|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal <window> (window-p <window>)))))

(test |(get-window-class-name <window>) = <class-name>|
  (with-fixture class ((string (gensym "WINCLASS")))
    (with-fixture window ((string (gensym "WIN")) :class-name <class-name>)
      (is (equal <class-name> (get-window-class-name <window>))))))

(test |(destroy-window <window> = t| 
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal t (destroy-window (w32api::%create-window <window-name>))))
    (is (eq nil (get-window <window-name>)))))

(test |(destroy-window <window>) should unregister class if class is created by w32api::%create-window| 
  (with-fixture window-name ((string (gensym "WIN")))
    (let ((<window> (w32api::%create-window <window-name>)))
      (destroy-window <window>))
    (is (not (equal 0 (register-class <window-name> (callback w32api::MainWndProc)))))
    (unregister-class <window-name>)))

(test |(destroy-window <window>) should not unregister class if class is not created by w32api::%create-window| 
  (with-fixture window-name ((string (gensym "WIN")))
    (with-fixture class (<window-name>)
      (destroy-window (w32api::%create-window <window-name>))
      (is (equal 0 (register-class <window-name> (callback w32api::MainWndProc)))))))

(test |(destroy-window <window>) should remove procedure registered in *message-handlers*| 
  (with-fixture window-name ((string (gensym "WIN")))
    (let* ((<window> (w32api::%create-window <window-name>)))
      (message-handler+ <window> nil (lambda (hWnd Msg lParam wParam) (declare (ignore hWnd Msg lParam wParam)) 0))
      (is (functionp (w32api::message-handler <window>)))
      (destroy-window <window>)
      (is (equal #'w32api.user32::DefWindowProcW (w32api::message-handler <window>))))))

(test |(destroy-window <window>) should remove class-name registered in *window-classes* if class is created by w32api::%create-window| 
  (with-fixture window-name ((string (gensym "WIN")))
    (let* ((<window> (w32api::%create-window <window-name>))
	   (key (pointer-address <window>)))
      (is (string-equal <window-name> (gethash key w32api::*window-classes*)))
      (destroy-window <window>)
      (is (equal nil (gethash key w32api::*window-classes*))))))

(test |(set-window-title <window>) = t and the title is changed|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (set-window-title <window> "Test Window")))
    (is (string-equal "Test Window" (get-window-title <window>)))
    ))

(test |(show-window <window>) = t| 
  (with-fixture window ((string (gensym "WIN")))
    (hide-window <window>)
    (is (equal t (show-window <window>)))
    (is (equal t (show-window <window>)))))

(test |(hide-window <window>) = t| 
  (with-fixture window ((string (gensym "WIN")))
    (show-window <window>)
    (is (equal t (hide-window <window>)))
    (is (equal t (hide-window <window>)))))

(test |(enable-window <window>) = t| 
  (with-fixture window ((string (gensym "WIN")))
    (disable-window <window>)
    (is (equal t (enable-window <window>)))
    (is (equal t (enable-window <window>)))))

(test |(disable-window <window>) = t| 
  (with-fixture window ((string (gensym "WIN")))
    (enable-window <window>)
    (is (equal t (disable-window <window>)))
    (is (equal t (disable-window <window>)))))

(test |(active-window <window>) = t| 
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (active-window <window>)))
    (is (equal t (active-window <window>)))))

(test |(switch-window <window>) = t and should set window active and focused|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (switch-window <window>)))
    (is (equal t (window-focused-p <window>)))
    (is (equal t (window-active-p <window>)))
					;(is (equal t (window-foregrounded-p <window-name>)))
    ))

(test |(focus-window <window>) = t| 
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (focus-window <window>)))
    (with-fixture window ((string (gensym "WIN")))
      (is (equal t (focus-window <window>)))
      (is (equal t (focus-window <window>))))))

(test |(foreground-window <window>) will set window foreground |
  (if *run-test-silently*
      (skip "set *run-test-silently* to nil if interactive tests are expected")
      (let ((result nil))
	(with-fixture window-name ((string (gensym "WIN")))
	  (let ((<window> 
		 (w32api::%create-window <window-name> :extended-style :topmost)))
	    (message-handler+ <window> nil
			      (lambda (hWnd x y z)
				(declare (ignore x y z))
				(cond ((foreground-window hWnd)
				       (setq result (window-foregrounded-p hWnd))
				       (post-quit-message 0)))
				0))
	    (show-window <window>)
	    (process-message <window>)
	    (destroy-window <window>)))
	
	(is (equal t result)))))

(test |(minimize-window <window>) = t| 
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (minimize-window <window>)))
    (is (equal t (minimize-window <window>)))))

(test |(maximize-window <window>) = t| 
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (maximize-window <window>)))
    (is (equal t (maximize-window <window>)))))

(test |(restore-window <window>) = t| 
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (restore-window <window>)))))

(test |(raise-window <window>) = t|
  (let* ((parent (create-window (string (gensym "WIN"))))
	 (child1 (create-window (string (gensym "CWIN")) :parent parent))
	 (child2 (create-window (string (gensym "CWIN")) :parent parent)))
    (is (eq t (raise-window child1)))
    (is (pointer-eq child1 (w32api.user32::GetTopWindow parent)))
    (is (eq t (raise-window child2)))
    (is (pointer-eq child2 (w32api.user32::GetTopWindow parent)))
    (destroy-window parent)))

(test |(bury-window <window>) = t|
  (let* ((parent (create-window (string (gensym "WIN"))))
	 (child1 (create-window (string (gensym "CWIN")) :parent parent))
	 (child2 (create-window (string (gensym "CWIN")) :parent parent)))
    (is (eq t (bury-window child1)))
    (is (pointer-eq child2 (w32api.user32::GetTopWindow parent)))
    (is (eq t (bury-window child2)))
    (is (pointer-eq child1 (w32api.user32::GetTopWindow parent)))
    (destroy-window parent)))

(test |(move-&-resize-window <window> 10 20 100 200) = t and move window to (0, 10)|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (move-&-resize-window <window> 10 20 100 200)))
    (is (equal 10 (first (multiple-value-list (get-window-rectangle <window>)))))
    (is (equal 20 (second (multiple-value-list (get-window-rectangle <window>)))))))

(test |(move-&-resize-window <window> 10 20 100 200) = (move-window <window> 10 20) and (resize-window <window> 100 200)|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (move-&-resize-window <window> 10 20 100 200)))		
    (let ((res (multiple-value-list (get-window-rectangle <window>))))
      (is (equal t (move-&-resize-window <window> 0 0 0 0)))
      (is (not (equal res (multiple-value-list (get-window-rectangle <window>)))))
      (is (equal t (move-window <window> 10 20)))
      (is (equal t (resize-window <window> 100 200)))
      (is (equal res (multiple-value-list (get-window-rectangle <window>)))))))

(test |(tile-windows <window>) = t and tile childrens in <windows>| 
  (is-true (tile-windows))
  (is-true (tile-windows :MDITILE_HORIZONTAL))
  (is-true (tile-windows :MDITILE_VERTICAL)))

(test |(cascade-windows <window>) = t and tile childrens in <windows>| 
  (is-true (cascade-windows))
  (is-true (cascade-windows :MDITILE_SKIPDISABLED))
  (is-true (cascade-windows :MDITILE_ZORDER)))

(test |(get-window-size <window>) = (values width height)|
  (with-fixture window ((string (gensym "WIN")))
    (multiple-value-bind (width height)
	(get-window-size <window>)
      (multiple-value-bind (x1 y1 x2 y2)
	  (get-window-rectangle <window>)
	(is (equal width (- x2 x1)))
	(is (equal height (- y2 y1)))))))

(test |(get-window-rectangle <window> t) is in area of (get-window-rectangle <window>)|
  (with-fixture window ((string (gensym "WIN")))
    (move-&-resize-window <window> 100 100 200 200)
    (multiple-value-bind (x1 y1 x2 y2)
	(get-window-rectangle <window> t)
      (is (eq 0 x1))			;Client Area always start from (0,0)
      (is (eq 0 y1))
      (multiple-value-bind (xx1 yy1 xx2 yy2)
	  (get-window-rectangle <window>)
	(is (eq 100 xx1))			;Top window start from screen top left
	(is (eq 100 yy1))
	(is (< (- x2 x1) (- xx2 xx1)))
	(is (< (- y2 y1) (- yy2 yy1)))))))

(test |(window-visible-p <window>) = t if the window is visible|
  (with-fixture window ((string (gensym "WIN")))
    (show-window <window>)
    (is (equal t (window-visible-p <window>)))))

(test |(window-visible-p <window>) = nil if the window is invisible|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal nil (window-visible-p <window>)))))

(test |(window-enabled-p <window>) = t if the window is enabled|
  (with-fixture window ((string (gensym "WIN")))
    (enable-window <window>)
    (is (equal t (window-enabled-p <window>)))))

(test |(window-enabled-p <window>) = nil if the window is disabled|
  (with-fixture window ((string (gensym "WIN")))
    (disable-window <window>)
    (is (equal nil (window-enabled-p <window>)))))

(test |(window-active-p <window>) = nil if the window is inactive|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal nil (window-active-p <window>)))))

(test |(window-active-p <window>) = t if the window is active|
  (with-fixture window ((string (gensym "WIN")))
    (active-window <window>)
    (is (equal t (window-active-p <window>)))))

(test |(window-active-p <window>) = nil if another window is activated|
  (with-fixture window ((string (gensym "WIN")))
    (let ((<previous-window> <window>))
      (with-fixture window ((string (gensym "WIN")))
	(active-window <previous-window>)
	(active-window <window>)
	(is (equal t (window-active-p <window>)))
	(is (equal nil (window-active-p <previous-window>)))))))

(test |(window-active-p <window>) = nil if another window is activated|
  (with-fixture window ((string (gensym "WIN")))
    (let ((<previous-window> <window>))
      (with-fixture window ((string (gensym "WIN")))
	(foreground-window <window>)
	(foreground-window <previous-window>)
	(is (equal nil (window-active-p <window>)))
	(is (equal t (window-active-p <previous-window>)))))))

(test |(window-foregrounded-p <window>) = nil if the window is not in foreground|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal nil (window-foregrounded-p <window>)))))

(test |(window-focused-p <window>) = nil if the window is not focused|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal nil (window-focused-p <window>)))))

(test |(window-focused-p <window>) = t if the window is focused|
  (with-fixture window ((string (gensym "WIN")))
    (focus-window <window>)
    (is (equal t (window-focused-p <window>)))
    (is (equal t (window-active-p <window>)))))

(test |(window-minimized-p <window>) = t if the window is minimized|
  (with-fixture window ((string (gensym "WIN")))
    (minimize-window <window>)
    (is (equal t (window-minimized-p <window>)))))

(test |(window-minimized-p <window>) = nil if the window is not minimized|
  (with-fixture window ((string (gensym "WIN")))
    (minimize-window <window>)
    (restore-window <window>)
    (is (equal nil (window-minimized-p <window>)))
    ))

(test |(window-maximized-p <window>) = t if the window is maximized|
  (with-fixture window ((string (gensym "WIN")))
    (maximize-window <window>)
    (is (equal t (window-maximized-p <window>)))))

(test |(window-maximized-p <window>) = nil if the window is not maximized|
  (with-fixture window ((string (gensym "WIN")))
    (maximize-window <window>)
    (restore-window <window>)
    (is (equal nil (window-maximized-p <window>)))))

(test |check state|
  (is (equal 0 (hash-table-count w32api::*window-classes*))) 
  (is (equal 0 (hash-table-count w32api::*message-handlers*))))

(test |multithread window creation/destroy test (low level)|
  (mapcar #'join-thread
	  (loop for index from 1 to 300 collect 
	       (make-thread
		(lambda ()
		  (let ((name (format nil "WIN~d" index))
			(parent-name (format nil "PWIN~d" index)))
		    (with-class (parent-name (callback w32api::MainWndProc))
		      (with-window (<parent-window> parent-name :class-name parent-name)
			(let ((<window> (w32api::%create-window name :parent <parent-window>)))
			  (message-handler+ <window> t
					    (lambda (hWnd Msg wParam lParam)
					      (declare (ignore Msg wParam lParam))
					      (with-drawing-context (dc hWnd)
						(declare (ignore dc)))
					      (post-quit-message 0)))
			  (show-window <parent-window>)
			  (show-window <window>)
			  (set-window-title <window> (format nil "CWIN~d" index))
			  (get-window-title <parent-window>)
			  (get-window-class-name <window>)
			  (window-active-p <window>)
			  (get-parent-window <window>)
			  (process-message)
			  (destroy-window <window>)))))))))
  (is (equal 0 (hash-table-count w32api::*window-classes*)))
  (is (equal 0 (hash-table-count w32api::*message-handlers*))))

(test |multithread window creation/destroy test (high level)|
  (mapcar
   (lambda (<window>)
     (w32api.user32::SendMessageW <window> :WM_CLOSE 0 0))
   (loop for index from 1 to 300 collect
	(let ((name (format nil "WIN~d" index)))
	  (let ((<window> (create-window name)))
	    (is (equal name (get-window-class-name <window>)))
	    (is (equal name (get-window-title <window>)))
	    (window-active-p <window>)
	    <window>))))
  (is (equal 0 (hash-table-count w32api::*window-classes*)))
  (is (equal 0 (hash-table-count w32api::*message-handlers*))))

(test |various window controls test|
  (with-fixture window-name ((string (gensym "WIN")))
    (let* ((w (create-window <window-name>))
	   (b (create-button "button1" w))
	   (c (create-checkbox "checkbox1" w))
	   (r (create-radiobox "radiobox1" w))
	   (g (create-groupbox "groupbox1" w))
	   (i (create-input w :text "input1"))
	   (e (create-editor w :text "editor1")))
      (is (window-p w))
      (is (window-p b))
      (is (equal (get-window-title b) "button1"))
      (is (window-p c))
      (is (equal (get-window-title c) "checkbox1"))
      (is (window-p r))
      (is (equal (get-window-title r) "radiobox1"))
      (is (window-p g))
      (is (equal (get-window-title g) "groupbox1"))
      (is (window-p i))
      (is (equal (get-window-title i) "input1"))
      (is (window-p e))
      (is (equal (get-window-title e) "editor1"))
      (destroy-window w)
      (is (not (window-p w)))
      (is (not (window-p b)))
      (is (not (window-p c)))
      (is (not (window-p r)))
      (is (not (window-p g)))
      (is (not (window-p i)))
      (is (not (window-p e)))
      (is (equal 0 (hash-table-count w32api::*window-classes*)))
      (is (equal 0 (hash-table-count w32api::*message-handlers*))))))

(test |(get-color-rgb (make-rgb-color 0 100 200)) = (values 0 100 200)|
  (multiple-value-bind (r g b)
      (get-color-rgb (make-rgb-color 0 100 200))
    (is (eq r 0))
    (is (eq g 100))
    (is (eq b 200))))

(test |(with-drawing-object ...) changes current drawing object|
  (with-fixture window ((string (gensym "WIN")))
    (WITH-DRAWING-CONTEXT (dc <window>)
      (let ((new-brush (w32api.gdi32::createSolidBrush (MAKE-RGB-COLOR 0 255 0)))
	    (new-pen (w32api.gdi32::CreatePen :PS_SOLID 1 (MAKE-RGB-COLOR 255 0 0)))
	    (old-brush (w32api.gdi32::GetCurrentObject dc :OBJ_BRUSH))
	    (old-pen (w32api.gdi32::GetCurrentObject dc :OBJ_PEN)))
	(let ((alived-new-brush (WITH-DRAWING-OBJECT (dc new-brush :old-object got-old-brush)
				  (is (not (pointer-eq new-brush got-old-brush)))
				  (is (pointer-eq old-brush got-old-brush))
				  (let ((alived-new-pen (WITH-DRAWING-OBJECT (dc new-pen :old-object got-old-pen :delete-p t)
							  (is (not (pointer-eq new-pen got-old-pen)))
							  (is (pointer-eq old-pen got-old-pen))
							  (w32api.gdi32::Rectangle dc 100 100 200 200))))
				    (is (not alived-new-pen))))))
	  (is (pointer-eq alived-new-brush new-brush)))
	(let ((alived-new-brush (WITH-DRAWING-OBJECT (dc new-brush :old-object new-old-brush :delete-p t)
				  (is (pointer-eq old-brush new-old-brush)))))
	  (is (not alived-new-brush)))))))

(test |(with-background-mode ...) changes current background mode|
  (with-fixture window ((string (gensym "WIN")))
    (WITH-DRAWING-CONTEXT (dc <window>)
      (let ((old (w32api.gdi32::GetBkMode dc)))
	(with-background-mode (dc (car (remove old '(:OPAQUE :TRANSPARENT))))
	  (is (not (equal old (w32api.gdi32::GetBkMode dc))))
	  (w32api.gdi32::TextOutW dc 100 100 "DRAW" 4))
	(is (equal old (w32api.gdi32::GetBkMode dc)))))))

(test |(set-text-color ...) will change current text color and (get-text-color ...) will get current color|
  (with-fixture window ((string (gensym "WIN")))
    (WITH-DRAWING-CONTEXT (dc <window>)
      (multiple-value-bind (r g b)
	  (get-text-color dc)
	(multiple-value-bind (rr gg bb)
	    (set-text-color dc :r 11 :g 22 :b 33)
	  (is (eq r rr))
	  (is (not (eq r 11)))
	  (is (eq g gg))
	  (is (not (eq g 22)))
	  (is (eq b bb))
	  (is (not (eq b 33))))
	(multiple-value-bind (rr gg bb)
	    (get-text-color dc)
	  (is (eq rr 11))
	  (is (eq gg 22))
	  (is (eq bb 33)))))))

(test |(set-background-color ...) will change current bk color and (get-background-color ...) will get current color|
  (with-fixture window ((string (gensym "WIN")))
    (WITH-DRAWING-CONTEXT (dc <window>)
      (multiple-value-bind (r g b)
	  (get-background-color dc)
	(multiple-value-bind (rr gg bb)
	    (set-background-color dc :r 33 :g 22 :b 11)
	  (is (eq r rr))
	  (is (not (eq r 33)))
	  (is (eq g gg))
	  (is (not (eq g 22)))
	  (is (eq b bb))
	  (is (not (eq b 11))))
	(multiple-value-bind (rr gg bb)
	    (get-background-color dc)
	  (is (eq rr 33))
	  (is (eq gg 22))
	  (is (eq bb 11)))))))

(test |(get-stock-object id) will return specified object |
  (loop for id in (foreign-enum-keyword-list 'w32api.type:STKOBJ_ENUM)
     do (is (not (null (get-stock-object id))))))

(test |(get-text-ascent ...) can get ascent of char in current font|
  (with-fixture window ((string (gensym "WIN")))
    (WITH-DRAWING-CONTEXT (dc <window>)
      (let ((a (get-text-ascent dc)))
	(is (numberp a))
	(is (not (eq a 0)))))))

(test |(get-text-descent ...) can get descent of char in current font|
  (with-fixture window ((string (gensym "WIN")))
    (WITH-DRAWING-CONTEXT (dc <window>)
      (let ((d (get-text-descent dc)))
	(is (numberp d))
	(is (not (eq d 0)))))))

(test |(get-text-height ...) can get height of char in current font|
  (with-fixture window ((string (gensym "WIN")))
    (WITH-DRAWING-CONTEXT (dc <window>)
      (let ((h (get-text-height dc)))
	(is (numberp h))
	(is (not (eq h 0)))
	(is (eq h (+ (get-text-descent dc) (get-text-ascent dc))))))))

(test |(get-text-char-width ...) can get average/max width of char in current font|
  (with-fixture window ((string (gensym "WIN")))
    (WITH-DRAWING-CONTEXT (dc <window>)
      (let ((w (get-text-char-width dc)))
	(is (numberp w))
	(is (not (eq w 0))))
      (let ((w (get-text-char-width dc :max-p t)))
	(is (numberp w))
	(is (not (eq w 0)))))))

(test |(get-text-extent ...) can get width and height of texts with current font|
  (with-fixture window ((string (gensym "WIN")))
    (WITH-DRAWING-CONTEXT (dc <window>)
      (let ((text "ABCD"))
	(multiple-value-bind (cx cy)
	    (get-text-extent dc text)
	  (is (numberp cx))
	  (is (numberp cy))
	  (is (not (eq cx 0)))
	  (is (not (eq cy 0)))
	  (is (> cx (* (length text) (get-text-char-width dc)))) ; assume that the sum of chars' avg width < text width, maybe im wrong
	  (is (eq cy (get-text-height dc))))))))

(test |(get-current-font ...) can get current font from dc|
  (with-fixture window ((string (gensym "WIN")))
    (WITH-DRAWING-CONTEXT (dc <window>)
      (let ((font (get-current-font dc)))
	(is (not (null-pointer-p font)))
	(is (listp (get-font-info font)))))))

(test |(create-font) return specified font object|
  (is (not (null-pointer-p (create-font))))
  (is (not (null-pointer-p (create-font :height 10))))
  (is (not (null-pointer-p (create-font :width 10))))
  (is (not (null-pointer-p (create-font :escapement 100.0))))
  (is (not (null-pointer-p (create-font :orientation 100.0))))
  (is (not (null-pointer-p (create-font :weight 100))))
  (is (not (null-pointer-p (create-font :weight :FW_HEAVY))))
  (is (not (null-pointer-p (create-font :italic t))))
  (is (not (null-pointer-p (create-font :underline t))))
  (is (not (null-pointer-p (create-font :strikeout t))))
  (is (not (null-pointer-p (create-font :charset :ANSI_CHARSET))))
  (is (not (null-pointer-p (create-font :precision '(:OUT_TT_PRECIS :CLIP_LH_ANGLES)))))
  (is (not (null-pointer-p (create-font :quality :CLEARTYPE_NATURAL_QUALITY))))
  (is (not (null-pointer-p (create-font :pitch :FIXED_PITCH))))
  (is (not (null-pointer-p (create-font :family :SWISS))))
  (is (not (null-pointer-p (create-font :face "custom font"))))
  )

(test |(get-font-info font) return font info|
  (let ((font-info (list
		    :height 10
		    :width 10
		    :escapement 100.0
		    :orientation 100.0
		    :weight :FW_HEAVY
		    :italic t
		    :underline t
		    :strikeout t
		    :charset :ANSI_CHARSET
		    :precision '(:OUT_TT_PRECIS :CLIP_LH_ANGLES)
		    :quality :CLEARTYPE_NATURAL_QUALITY
		    :pitch :FIXED_PITCH
		    :family :SWISS
		    :face "custom font")))
    (let ((got-font-info (get-font-info (apply #'create-font font-info))))
      (equal font-info got-font-info))))

(test |(get-all-font-infos dc) get all available font infos in dc|
  (with-fixture window ((string (gensym "WIN")))
    (WITH-DRAWING-CONTEXT (dc <window>)
      (let ((font-infos (get-all-font-infos dc)))
	(is (> (length font-infos) 0))
	(let ((got-font-infos
	       (loop for info in font-infos collect
		    (progn
		      (let ((font (apply #'create-font info)))
			(unwind-protect
			     (get-font-info font)
			  (destroy-font font)))))))
	  (is (equal (length font-infos) (length got-font-infos)))
	  (is (equal font-infos got-font-infos)))))))

(test |(destroy-font font) will destroy the created font|
  (is-true (destroy-font (create-font))))

(test |(create-pen) return specified pen object|
  (is (not (null-pointer-p (create-pen))))
  (is (not (null-pointer-p (create-pen :style :PS_DOT))))
  (is (not (null-pointer-p (create-pen :type :PS_COSMETIC))))
  (is (not (null-pointer-p (create-pen :style '(1 2 3) :endcap :PS_ENDCAP_ROUND))))
  (is (not (null-pointer-p (create-pen :color '(0 0 1)))))
  (is (not (null-pointer-p (create-pen :brush :BS_HATCHED :hatch :HS_CROSS))))
  (is (not (null-pointer-p (create-pen :width 3))))
  (is (not (null-pointer-p (create-pen :join :PS_JOIN_ROUND)))))

(test |(get-pen-info) return specified pen info from pen|
  (let ((pen-info (get-pen-info (create-pen))))
    (is (equal pen-info (get-pen-info (apply #'create-pen pen-info)))))
  (let ((pen-info (get-pen-info (create-pen :style :PS_DOT))))
    (is (equal pen-info (get-pen-info (apply #'create-pen pen-info)))))
  (let ((pen-info (get-pen-info (create-pen :type :PS_COSMETIC))))
    (is (equal pen-info (get-pen-info (apply #'create-pen pen-info)))))
  (let ((pen-info (get-pen-info (create-pen :style '(1 2 3) :endcap :PS_ENDCAP_ROUND))))
    (is (equal pen-info (get-pen-info (apply #'create-pen pen-info)))))
  (let ((pen-info (get-pen-info (create-pen :brush :BS_HATCHED :hatch :HS_CROSS))))
    (is (equal pen-info (get-pen-info (apply #'create-pen pen-info)))))
  (let ((pen-info (get-pen-info (create-pen :width 3))))
    (is (equal pen-info (get-pen-info (apply #'create-pen pen-info)))))
  (let ((pen-info (get-pen-info (create-pen :join :PS_JOIN_ROUND))))
    (is (equal pen-info (get-pen-info (apply #'create-pen pen-info)))))
  )

(test |(destroy-pen pen) will destroy the pen created|
  (is-true (destroy-pen (create-pen))))

(test |(create-brush) will create specified brush|
  (is (not (null-pointer-p (create-brush))))
  (is (not (null-pointer-p (create-brush :style :BS_HATCHED))))
  (is (not (null-pointer-p (create-brush :color '(255 255 255)))))
  (is (not (null-pointer-p (create-brush :hatch :HS_CROSS)))))

(test |(get-brush-info) will get specified brush info|
  (let ((brush-info (get-brush-info (create-brush))))
    (is (equal brush-info (get-brush-info (apply #'create-brush brush-info)))))
  (let ((brush-info (get-brush-info (create-brush :style :BS_HATCHED))))
    (is (equal brush-info (get-brush-info (apply #'create-brush brush-info)))))
  (let ((brush-info (get-brush-info (create-brush :color '(255 255 255)))))
    (is (equal brush-info (get-brush-info (apply #'create-brush brush-info)))))
  (let ((brush-info (get-brush-info (create-brush :hatch :HS_CROSS))))
    (is (equal brush-info (get-brush-info (apply #'create-brush brush-info))))))

(test |(destroy-brush brush will destroy brush|
  (is-true (destroy-brush (create-brush))))
