(in-package #:w32api/test)

(def-suite test/w32api :in test)

(in-suite test/w32api)

;;;; kernel32 api
(test |(get-error) = 0 if no errors|
  (is (equal 0 (get-error))))

(test |(print-error 0) = string indicates no errors|
  (is (search "The operation completed successfully." (print-error 0)))) ;fixme : suppose the locale is en by defualt.

;;; user32 api
(def-fixture class-name (<class-name>)
  (print <class-name>)
  (&body))

(def-fixture window-name (<window-name>)
  (print <window-name>)
  (&body))

(def-fixture class (<class-name>)
  (print <class-name>)
  (with-class (<class-name>)
    (&body)))

(def-fixture window (<window-name>
		     &key
		     ((:class-name <class-name>) <window-name>)
		     ((:parent <parent-window>) (null-pointer)))
  (print <window-name>)
  (with-window (<window> <window-name> :class-name <class-name> :parent <parent-window>)
    (&body)))

(test |(get-current-desktop) return current desktop|
  (is (not (null-pointer-p (get-current-desktop)))))

(test |(create-desktop <name>) return new created desktop|
  (let ((desk (create-desktop (string (gensym "DESK")))))
    (is (not (null-pointer-p desk)))
    (is-true (destroy-desktop desk))))

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

(test |call on <invalid-desktop> should return nil| 
  (dolist (func (list
		 #'switch-desktop
		 #'destroy-desktop		 
		 ))
    (is (equal nil (funcall func (null-pointer))))
    (is (equal nil (funcall func (make-pointer #x1))))
    (with-foreign-object (invalid :int)
      (is (equal nil (funcall func invalid)))))
  )

(test |(register-class <new-name>) /= 0 to indicate no errors|
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (is (not (equal 0 (register-class <class-name>))))
    (unregister-class <class-name>)))

(test |(register-class <exist-name>) = 0 to indicate error|
  (with-fixture class ((string (gensym "WINCLASS")))
    (is (equal 0 (register-class <class-name>)))))

(test |(unregister-class <new-name>) = nil| 
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (is (equal nil (unregister-class <class-name>)))))

(test |(unregister-class <exist-name>) = t| 
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (register-class <class-name>)
    (is (equal t (unregister-class <class-name>)))))

(test |(create-window <new-name>) = <window>| 
  (with-fixture window-name ((string (gensym "WIN")))
    (let ((<window> (create-window <window-name>)))
      (is-true (window-p <window>))
      (destroy-window <window>))))

(test |(create-window <new-name> :class-name <new-name>) = <window>| 
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (with-fixture window-name ((string (gensym "WIN")))
      (let ((<window> (create-window <window-name> :class-name <class-name>)))
	(is-true (window-p <window>))
	(is (equal <class-name> (get-window-class-name <window>)))
	(destroy-window <window>)))))

(test |(create-window <new-name> :class-name <exist-name>) = <window>| 
  (with-fixture class ((string (gensym "WINCLASS")))
    (with-fixture window-name ((string (gensym "WIN")))
      (let ((<window> (create-window <window-name> :class-name <class-name>)))
	(is-true (window-p <window>))
	(is (equal <class-name> (get-window-class-name <window>)))
	(destroy-window <window>)))))

(test |(create-window <new-name> :parent <exist-window>) = <window>| 
  (with-fixture window ((string (gensym "WIN")))
    (with-fixture window-name ((string (gensym "WIN")))
      (let ((<child-window> (create-window <window-name> :parent <window>)))
	(is-true (window-p <child-window>))
	(is-true (parent-window-p <child-window> <window>))
	(destroy-window <child-window>)))))

(test |(create-window <exist-name>) = nil| 
  (with-fixture window ((string (gensym "WIN")))
    (is-false (create-window <window-name>))))

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
	(is (eq nil (get-window <window-name>)))
	(is (pointer-eq <window> (get-window <window-name> :parent <parent-window>)))
	))))

(test |call on <invalid-window> should return nil| 
  (dolist (func (list
		 #'window-p
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
    (is (member :WS_CHILD (get-window-style <window>))))
  )

(test |(set-window-style <window> <style>) will set <style> to <window>|
  (with-fixture window ((string (gensym "WIN")))
    (set-window-style <window> (list :WS_CHILD :BS_CHECKBOX))
    (is (member :WS_CHILD (get-window-style <window>)))
    (is (member :BS_CHECKBOX (get-window-style <window>))))
  )

(test |(get-parent-window <window>) will return parent of <window>|
  (with-fixture window ((string (gensym "WIN")))
    (with-fixture window ((string (gensym "WIN")) :parent <window>)
      (is (pointer-eq <parent-window> (get-parent-window <window>))))))

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
    (let ((children (loop repeat 10 collect (create-window (string (gensym "WIN")) :parent <window>))))
      (is (pointer-eq (first children) (get-child-window <window>)))
      (is (pointer-eq (first (last children)) (get-child-window <window> :reverse t)))
      (mapc #'destroy-window children))))

(test |(get-child-window <window> :nth <n>) will return nth child if n < child count|
  (with-fixture window ((string (gensym "WIN")))
    (let ((children (loop repeat 10 collect (create-window (string (gensym "WIN")) :parent <window>))))
      (is (pointer-eq (third children) (get-child-window <window> :nth 3)))
      (is (pointer-eq (third children) (get-child-window <window> :nth 8 :reverse t)))
      (mapc #'destroy-window children))))

(test |(get-child-window <window> :nth <n>) will return nil if n > child count or < 1|
  (with-fixture window ((string (gensym "WIN")))
    (is (eq nil (get-child-window <window>)))
    (let ((children (loop repeat 10 collect (create-window (string (gensym "WIN")) :parent <window>))))
      (is (eq nil (get-child-window <window> :nth 11)))
      (is (eq nil (get-child-window <window> :nth 11 :reverse t)))
      (is (eq nil (get-child-window <window> :nth 0)))
      (is (eq nil (get-child-window <window> :nth 0 :reverse t)))
      (mapc #'destroy-window children))))

(test |(get-children-windows <window>) will return all children windows|
  (with-fixture window ((string (gensym "WIN")))
    (is (eq nil (get-children-windows <window>)))
    (let ((children (loop repeat 10 collect (create-window (string (gensym "WIN")) :parent <window>))))
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
      (let ((children (loop repeat 10 collect (create-window (string (gensym "WIN")) :parent <window>))))
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
    (is (equal t (destroy-window (create-window <window-name>))))
    (is (eq nil (get-window <window-name>)))))

(test |(destroy-window <window>) should unregister class if class is created by create-window| 
  (with-fixture window-name ((string (gensym "WIN")))
    (let ((<window> (create-window <window-name>)))
      (destroy-window <window>))
    (is (not (equal 0 (register-class <window-name>))))
    (unregister-class <window-name>)))

(test |(destroy-window <window>) should not unregister class if class is not created by create-window| 
  (with-fixture window-name ((string (gensym "WIN")))
    (with-fixture class (<window-name>)
      (destroy-window (create-window <window-name>))
      (is (equal 0 (register-class <window-name>))))))

(test |(destroy-window <window>) should remove procedure registered in *create-window-owned-procedures*| 
  (with-fixture window-name ((string (gensym "WIN")))
    (let* ((<window> (create-window <window-name> :procedure (lambda (hWnd Msg lParam wParam cont) (declare (ignore hWnd Msg lParam wParam cont)) 0)))
	   (key (pointer-address <window>)))
      (is (functionp (gethash key w32api::*create-window-owned-procedures*)))
      (destroy-window <window>)
      (is (equal nil (gethash key w32api::*create-window-owned-procedures*))))))

(test |(destroy-window <window>) should remove class-name registered in *create-window-owned-classes* if class is created by create-window| 
  (with-fixture window-name ((string (gensym "WIN")))
    (let* ((<window> (create-window <window-name>))
	   (key (pointer-address <window>)))
      (is (string-equal <window-name> (gethash key w32api::*create-window-owned-classes*)))
      (destroy-window <window>)
      (is (equal nil (gethash key w32api::*create-window-owned-classes*))))))

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
		 (create-window <window-name>
				:extended-style :topmost
				:procedure (lambda (hWnd x y z c)
					     (declare (ignore x y z c))
					     (cond ((foreground-window hWnd)
						    (setq result (window-foregrounded-p hWnd))
						    (post-quit-message 0))
						   (t 0))))))
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

(test |(move-window <window> 10 20 100 200) = t and move window to (0, 10)|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (move-window <window> 10 20 100 200)))
    (is (equal 10 (first (multiple-value-list (get-window-rectangle <window>)))))
    (is (equal 20 (second (multiple-value-list (get-window-rectangle <window>)))))))

(test |(get-window-size <window>) = (values width height)|
  (with-fixture window ((string (gensym "WIN")))
    (multiple-value-bind (width height)
	(get-window-size <window>)
      (multiple-value-bind (x1 y1 x2 y2)
	  (get-window-rectangle <window>)
	(is (equal width (- x2 x1)))
	(is (equal height (- y2 y1)))))))

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
  (is (equal 0 (hash-table-count w32api::*create-window-owned-classes*))) 
  (is (equal 0 (hash-table-count w32api::*create-window-owned-procedures*))))

(test |multithread window creation/destroy test|
  (let ((*kernel* (lparallel:make-kernel 100)))
    (pmapc (lambda (index)
	     (let ((name (format nil "WIN~d" index))
		   (parent-name (format nil "PWIN~d" index)))
	       (with-class (parent-name)
		 (with-window (<parent-window> parent-name :class-name parent-name)
		   (let ((<window>
			  (create-window name
					 :parent <parent-window>
					 :procedure
					 (lambda (hWnd Msg lParam wParam cont)
					   (declare (ignore Msg lParam wParam cont))
					   (with-drawing-context (dc hWnd)
					     (declare (ignore dc)))
					   (post-quit-message 0)))))
		     (show-window <parent-window>)
		     (show-window <window>)
		     (set-window-title <window> (format nil "CWIN~d" index))
		     (get-window-title <parent-window>)
		     (get-window-class-name <window>)
		     (window-active-p <window>)
		     (get-parent-window <window>)
		     (process-message)
		     (destroy-window <window>))))))
	   (loop for x from 1 to 300 collect x)))
  (is (equal 0 (hash-table-count w32api::*create-window-owned-classes*)))
  (is (equal 0 (hash-table-count w32api::*create-window-owned-procedures*))))
