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

(def-fixture class (<class-name>)
  (print <class-name>)
  (register-class <class-name>)
  (&body)
  (unregister-class <class-name>))

(def-fixture window-name (<window-name>)
  (print <window-name>)
  (&body))

(def-fixture window (<window-name> &optional (<class-name> <window-name>))
  (print <window-name>)
  (create-window <window-name> :class-name <class-name>)
  (&body)
  (destroy-window <window-name> :class-name <class-name>))

(test |(register-class <new-name>) = non-zero to indicate no errors|
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (is (not (equal 0 (register-class <class-name>))))
    (unregister-class <class-name>)))

(test |(register-class <exist-name>) = zero to indicate error|
  (with-fixture class ((string (gensym "WINCLASS")))
    (is (equal 0 (register-class <class-name>)))))

(test |(unregister-class <new-name>) = nil| 
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (is (equal nil (unregister-class <class-name>)))))

(test |(unregister-class <exist-name>) = t| 
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (register-class <class-name>)
    (is (equal t (unregister-class <class-name>)))))

(test |(unregister-class <exist-name>) = t| 
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (register-class <class-name>)
    (is (equal t (unregister-class <class-name>)))))

(test |(create-window <new-name>) = <window-handle>| 
  (with-fixture window-name ((string (gensym "WIN")))
    (is-true (window-p (create-window <window-name>)))
    (destroy-window <window-name>)))

(test |(create-window <new-name> :class-name <new-name>) = <window-handle>| 
  (with-fixture class-name ((string (gensym "WINCLASS")))
    (with-fixture window-name ((string (gensym "WIN")))
      (is-true (window-p (create-window <window-name> :class-name <class-name>)))
      (destroy-window <window-name> :class-name <class-name>))))

(test |(create-window <new-name> :class-name <exist-name>) = <window-handle>| 
  (with-fixture class ((string (gensym "WINCLASS")))
    (with-fixture window-name ((string (gensym "WIN")))
      (is-true (window-p (create-window <window-name> :class-name <class-name>)))
      (destroy-window <window-name>))))

(test |(create-window <exist-name>) = nil| 
  (with-fixture window ((string (gensym "WIN")))
    (is-false (create-window <window-name>))))

(test |(window-p <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal nil (window-p <window-name>)))))

(test |(get-window <window-name>) = <window-handle>|
  (with-fixture window ((string (gensym "WIN")))
    (is (window-p (get-window <window-name> :class-name <window-name>)))))

(test |(window-p <exist-name>) = <window-handle>|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (pointerp (window-p <window-name>))))))

(test |(get-window-class-name (null-pointer)) = nil|
  (is (equal nil (get-window-class-name (null-pointer)))))

(test |(get-window-class-name <invalid-handle>) = nil|
  (is (equal nil (get-window-class-name (make-pointer #x1)))))

(test |(get-window-class-name <window-handle>) = <class-name>|
  (with-fixture class ((string (gensym "WINCLASS")))
    (with-fixture window ((string (gensym "WIN")) <class-name>)
      (is (equal <class-name> (get-window-class-name (get-window <window-name> :class-name <class-name>)))))))

(test |(destroy-window <new-name>) = nil| 
  (with-fixture window-name ((string (gensym "WIN")))
    (is-false (destroy-window <window-name>))))

(test |(destroy-window <exist-name>) = t| 
  (with-fixture window-name ((string (gensym "WIN")))
    (create-window <window-name>)
    (is (equal t (destroy-window <window-name>)))))

(test |(destroy-window <name>) should unregister class if class is created by create-window| 
  (with-fixture window-name ((string (gensym "WIN")))
    (create-window <window-name>)
    (destroy-window <window-name>)
    (is (not (equal 0 (register-class <window-name>))))
    (unregister-class <window-name>)))

(test |(destroy-window <name>) should not unregister class if class is not created by create-window| 
  (with-fixture window-name ((string (gensym "WIN")))
    (with-fixture class (<window-name>)
      (create-window <window-name>)
      (destroy-window <window-name>)
      (is (equal 0 (register-class <window-name>))))))

(test |(destroy-window <exist-name>) should remove procedure registered in *create-window-owned-procedures*| 
  (with-fixture window-name ((string (gensym "WIN")))
    (create-window <window-name>)
    (let ((key (pointer-address (w32api.user32::FindWindowExA (null-pointer) (null-pointer) <window-name> <window-name>))))
      (destroy-window <window-name>)
      (is (equal nil (gethash key w32api::*create-window-owned-procedures*))))))

(test |(destroy-window <exist-name>) should remove class-name registered in *create-window-owned-classes* if class is created by create-window| 
  (with-fixture window-name ((string (gensym "WIN")))
    (create-window <window-name>)
    (let ((key (pointer-address (w32api.user32::FindWindowExA (null-pointer) (null-pointer) <window-name> <window-name>))))
      (destroy-window <window-name>)
      (is (equal nil (gethash key w32api::*create-window-owned-classes*))))))

(test |(show-window <new-name>) = nil| 
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal nil (show-window <window-name>)))))

(test |(show-window <exist-name>) = t if the window has been hidden| 
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (show-window <window-name>)))))

(test |(show-window <exist-name>) = nil if the window has been shown| 
  (with-fixture window ((string (gensym "WIN")))
    (show-window <window-name>)
    (is (equal nil (show-window <window-name>)))))

(test |(hide-window <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal nil (hide-window <window-name>)))))

(test |(hide-window <exist-name>) = t if the window has been shown| 
  (with-fixture window ((string (gensym "WIN")))
    (show-window <window-name>)
    (is (equal t (hide-window <window-name>)))))

(test |(hide-window <exist-name>) = nil if the window has been hidden| 
  (with-fixture window ((string (gensym "WIN")))
    (is (equal nil (hide-window <window-name>)))))

(test |(enable-window <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal nil (enable-window <window-name>)))))

(test |(enable-window <exist-name>) = t if the window has been disabled| 
  (with-fixture window ((string (gensym "WIN")))
    (disable-window <window-name>)
    (is (equal t (enable-window <window-name>)))))

(test |(enable-window <exist-name>) = nil if the window has been enabled| 
  (with-fixture window ((string (gensym "WIN")))
    (enable-window <window-name>)
    (is (equal nil (enable-window <window-name>)))))

(test |(disable-window <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal nil (disable-window <window-name>)))))

(test |(disable-window <exist-name>) = nil if the window has been disabled| 
  (with-fixture window ((string (gensym "WIN")))
    (disable-window <window-name>)
    (is (equal nil (disable-window <window-name>)))))

(test |(disable-window <exist-name>) = t if the window has been enabled| 
  (with-fixture window ((string (gensym "WIN")))
    (enable-window <window-name>)
    (is (equal t (disable-window <window-name>)))))

(test |(active-window <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal nil (active-window <window-name>)))))

(test |(active-window <exist-name>) = t| 
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (active-window <window-name>)))))

(test |(focus-window <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal nil (focus-window <window-name>)))))

(test |(focus-window <exist-name>) = t| 
  (with-fixture window ((string (gensym "WIN")))
    (is (equal t (focus-window <window-name>)))
    (with-fixture window ((string (gensym "WIN")))
      (is (equal t (focus-window <window-name>))))))

(test |(foreground-window <exist-name>) will set window foreground |
  (if *run-test-silently*
      (skip "set *run-test-silently* to nil if interactive tests are expected")
      (let ((result nil))
	(with-fixture window-name ((string (gensym "WIN")))
	  (create-window <window-name>
			 :extended-style :topmost
			 :procedure (lambda (hWnd x y z)
				      (declare (ignore x y z))
				      (cond ((foreground-window hWnd)
					     (setq result (window-foregrounded-p hWnd))
					     (post-quit-message hWnd)))))
	  (show-window <window-name>)
	  (process-message <window-name>)
	  (destroy-window <window-name>))
	
	(is (equal t result)))))

(test |(window-visible-p <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal nil (window-visible-p <window-name>)))))

(test |(window-visible-p <exist-name>) = t if the window is visible|
  (with-fixture window ((string (gensym "WIN")))
    (show-window <window-name>)
    (is (equal t (window-visible-p <window-name>)))))

(test |(window-visible-p <exist-name>) = nil if the window is invisible|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal nil (window-visible-p <window-name>)))))

(test |(window-enabled-p <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal nil (window-enabled-p <window-name>)))))

(test |(window-enabled-p <exist-name>) = t if the window is enabled|
  (with-fixture window ((string (gensym "WIN")))
    (enable-window <window-name>)
    (is (equal t (window-enabled-p <window-name>)))))

(test |(window-enabled-p <exist-name>) = nil if the window is disabled|
  (with-fixture window ((string (gensym "WIN")))
    (disable-window <window-name>)
    (is (equal nil (window-enabled-p <window-name>)))))

(test |(window-active-p <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal nil (window-active-p <window-name>)))))

(test |(window-active-p <exist-name>) = nil if the window is inactive|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal nil (window-active-p <window-name>)))))

(test |(window-active-p <exist-name>) = t if the window is active|
  (with-fixture window ((string (gensym "WIN")))
    (active-window <window-name>)
    (is (equal t (window-active-p <window-name>)))))

(test |(window-active-p <exist-name>) = nil if another window is activated|
  (with-fixture window ((string (gensym "WIN")))
    (let ((previous-name <window-name>))
      (with-fixture window ((string (gensym "WIN")))
	(active-window previous-name)
	(active-window <window-name>)
	(is (equal t (window-active-p <window-name>)))
	(is (equal nil (window-active-p previous-name)))))))

(test |(window-active-p <exist-name>) = nil if another window is activated|
  (with-fixture window ((string (gensym "WIN")))
    (let ((previous-name <window-name>))
      (with-fixture window ((string (gensym "WIN")))
	(foreground-window <window-name>)
	(foreground-window previous-name)
	(is (equal nil (window-active-p <window-name>)))
	(is (equal t (window-active-p previous-name)))))))

(test |(window-foregrounded-p <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal nil (window-foregrounded-p <window-name>)))))

(test |(window-foregrounded-p <exist-name>) = nil if the window is not in foreground|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal nil (window-foregrounded-p <window-name>)))))

(test |(window-focused-p <new-name>) = nil if the window is not focused|
  (with-fixture window-name ((string (gensym "WIN")))
    (is (equal nil (window-focused-p <window-name>)))))

(test |(window-focused-p <exist-name>) = nil if the window is not focused|
  (with-fixture window ((string (gensym "WIN")))
    (is (equal nil (window-focused-p <window-name>)))))

(test |(window-focused-p <exist-name>) = t if the window is focused|
  (with-fixture window ((string (gensym "WIN")))
    (focus-window <window-name>)
    (is (equal t (window-focused-p <window-name>)))
    (is (equal t (window-active-p <window-name>)))))

(test |check state|
  (is (equal 0 (hash-table-count w32api::*create-window-owned-classes*))) 
  (is (equal 0 (hash-table-count w32api::*create-window-owned-procedures*))))

(test |multithread window creation/destroy test|
  (let ((*kernel* (lparallel:make-kernel 100)))
    (pmapc (lambda (index)		; fixme: when the worker will be freed?
	     (let ((name (format nil "WIN~d" index)))
	       (print name)
	       (create-window name :procedure (lambda (hWnd b c d) (declare (ignore hWnd b c d))))
	       (show-window name)
	       (destroy-window name)))
	   (loop for x from 1 to 100 collect x)))
  (is (equal 0 (hash-table-count w32api::*create-window-owned-classes*)))
  (is (equal 0 (hash-table-count w32api::*create-window-owned-procedures*))))
