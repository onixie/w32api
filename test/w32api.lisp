(in-package #:w32api/test)

(def-suite test/w32api :in test)

(in-suite test/w32api)

					; kernel32 api
(test |(get-error) = 0 if no errors|
  (is (equal 0 (get-error))))

(test |(print-error 0) = string indicates no errors|
  (is (search "The operation completed successfully." (print-error 0)))) ;fixme : suppose the locale is en by defualt.

					; user32 api
(def-fixture class-name (<class-name>)
  (&body))

(def-fixture class (<class-name>)
  (register-class <class-name>)
  (&body)
  (unregister-class <class-name>))

(def-fixture window-name (<window-name>)
  (&body))

(def-fixture window (<window-name> &optional (<class-name> <window-name>))
  (create-window <window-name> :class-name <class-name>)
  (&body)
  (destroy-window <window-name> :class-name <class-name>))

;;; 
(test |(register-class <new-name>) = non-zero to indicate no errors|
  (with-fixture class-name ((string (gensym "WINCLASS-TEST-")))
    (is (not (equal 0 (register-class <class-name>))))
    (unregister-class <class-name>)))

(test |(register-class <exist-name>) = zero to indicate error|
  (with-fixture class ((string (gensym "WINCLASS-TEST-")))
    (is (equal 0 (register-class <class-name>)))))

(test |(unregister-class <new-name>) = nil| 
  (with-fixture class-name ((string (gensym "WINCLASS-TEST-")))
    (is (equal nil (unregister-class <class-name>)))))

(test |(unregister-class <exist-name>) = t| 
  (with-fixture class-name ((string (gensym "WINCLASS-TEST-")))
    (register-class <class-name>)
    (is (equal t (unregister-class <class-name>)))))

(test |(unregister-class <exist-name>) = t| 
  (with-fixture class-name ((string (gensym "WINCLASS-TEST-")))
    (register-class <class-name>)
    (is (equal t (unregister-class <class-name>)))))

(test |(create-window <new-name>) = <window-handle>| 
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (is-true (window-p (create-window <window-name>)))
    (destroy-window <window-name>)))

(test |(create-window <new-name> :class-name <new-name>) = <window-handle>| 
  (with-fixture class-name ((string (gensym "WINCLASS-TEST-")))
    (with-fixture window-name ((string (gensym "WIN-TEST-")))
      (is-true (window-p (create-window <window-name> :class-name <class-name>)))
      (destroy-window <window-name>))))

(test |(create-window <new-name> :class-name <exist-name>) = <window-handle>| 
  (with-fixture class ((string (gensym "WINCLASS-TEST-")))
    (with-fixture window-name ((string (gensym "WIN-TEST-")))
      (is-true (window-p (create-window <window-name> :class-name <class-name>)))
      (destroy-window <window-name>))))

(test |(create-window <exist-name>) = nil| 
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (is-false (create-window <window-name>))))

(test |(destroy-window <new-name>) = nil| 
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (is-false (destroy-window <window-name>))))

(test |(destroy-window <exist-name>) = t| 
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (create-window <window-name>)
    (is (equal t (destroy-window <window-name>)))))

(test |(destroy-window <name>) should unregister class if class is created by create-window| 
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (create-window <window-name>)
    (destroy-window <window-name>)
    (is (not (equal 0 (register-class <window-name>))))
    (unregister-class <window-name>)))

(test |(destroy-window <name>) should not unregister class if class is not created by create-window| 
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (with-fixture class (<window-name>)
      (create-window <window-name>)
      (destroy-window <window-name>)
      (is (equal 0 (register-class <window-name>))))))

(test |(destroy-window <exist-name>) should remove procedure registered in *create-window-owned-procedures*| 
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (create-window <window-name>)
    (let ((key (pointer-address (w32api.user32::FindWindowExA (null-pointer) (null-pointer) <window-name> <window-name>))))
      (destroy-window <window-name>)
      (is (equal nil (gethash key w32api::*create-window-owned-procedures*))))))

(test |(destroy-window <exist-name>) should remove class-name registered in *create-window-owned-classes* if class is created by create-window| 
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (create-window <window-name>)
    (let ((key (pointer-address (w32api.user32::FindWindowExA (null-pointer) (null-pointer) <window-name> <window-name>))))
      (destroy-window <window-name>)
      (is (equal nil (gethash key w32api::*create-window-owned-classes*))))))

(test |(show-window <new-name>) = nil| 
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (is (equal nil (show-window <window-name>)))))

(test |(show-window <exist-name>) = t if the window has been hidden| 
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (is (equal t (show-window <window-name>)))))

(test |(show-window <exist-name>) = nil if the window has been shown| 
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (show-window <window-name>)
    (is (equal nil (show-window <window-name>)))))

(test |(hide-window <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (is (equal nil (hide-window <window-name>)))))

(test |(hide-window <exist-name>) = t if the window has been shown| 
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (show-window <window-name>)
    (is (equal t (hide-window <window-name>)))))

(test |(hide-window <exist-name>) = nil if the window has been hidden| 
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (is (equal nil (hide-window <window-name>)))))

(test |(enable-window <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (is (equal nil (enable-window <window-name>)))))

(test |(enable-window <exist-name>) = t if the window has been disabled| 
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (disable-window <window-name>)
    (is (equal t (enable-window <window-name>)))))

(test |(enable-window <exist-name>) = nil if the window has been enabled| 
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (enable-window <window-name>)
    (is (equal nil (enable-window <window-name>)))))

(test |(disable-window <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (is (equal nil (disable-window <window-name>)))))

(test |(disable-window <exist-name>) = nil if the window has been disabled| 
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (disable-window <window-name>)
    (is (equal nil (disable-window <window-name>)))))

(test |(disable-window <exist-name>) = t if the window has been enabled| 
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (enable-window <window-name>)
    (is (equal t (disable-window <window-name>)))))

(test |(window-p <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
      (is (equal nil (window-p <window-name>)))))

(test |(window-p <exist-name>) = <window-handle>|
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (is (equal t (pointerp (window-p <window-name>))))))

(test |(window-visible-p <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (is (equal nil (window-visible-p <window-name>)))))

(test |(window-visible-p <exist-name>) = t if the window is visible|
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (show-window <window-name>)
    (is (equal t (window-visible-p <window-name>)))))

(test |(window-visible-p <exist-name>) = nil if the window is invisible|
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (is (equal nil (window-visible-p <window-name>)))))

(test |(window-enabled-p <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (is (equal nil (window-enabled-p <window-name>)))))

(test |(window-enabled-p <exist-name>) = t if the window is enabled|
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (enable-window <window-name>)
    (is (equal t (window-enabled-p <window-name>)))))

(test |(window-enabled-p <exist-name>) = nil if the window is disabled|
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (disable-window <window-name>)
    (is (equal nil (window-enabled-p <window-name>)))))

(test |(window-active-p <new-name>) = nil|
  (with-fixture window-name ((string (gensym "WIN-TEST-")))
    (is (equal nil (window-active-p <window-name>)))))

(test |(window-active-p <exist-name>) = nil if the window is inactive|
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (is (equal nil (window-active-p <window-name>)))))

(test |(window-active-p <exist-name>) = t if the window is active|
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (active-window <window-name>)
    (is (equal t (window-active-p <window-name>)))))

(test |(get-window-class-name (null-pointer)) = nil|
  (is (equal nil (get-window-class-name (null-pointer)))))

(test |(get-window-class-name <invalid-handle>) = nil|
  (is (equal nil (get-window-class-name (make-pointer #x1)))))

(test |(get-window-class-name <window-handle>) = <class-name>|
  (with-fixture class ((string (gensym "WINCLASS-TEST-")))
    (with-fixture window ((string (gensym "WIN-TEST-")) <class-name>)
      (is (equal <class-name> (get-window-class-name (get-window <window-name> :class-name <class-name>)))))))

(test |(get-window <window-name>) = <window-handle>|
  (with-fixture window ((string (gensym "WIN-TEST-")))
    (is (window-p (get-window <window-name> :class-name <window-name>)))))
