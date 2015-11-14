(in-package #:w32api/test)

(def-suite test/w32api :in test)

(in-suite test/w32api)

; kernel32 api
(test |(get-error) should return 0 if no errors|
  (is (equal 0 (get-error))))

(test |(print-error 0) should return string indicates no errors|
  (is (search "The operation completed successfully." (print-error 0)))) ;fixme : suppose the locale is en by defualt.

; user32 api
(def-fixture class-name (<class-name>)
  (&body)
  (unregister-class <class-name>))

(def-test |(register-class <new-name>) should return non-zero to indicate no errors| ()
  (with-fixture class-name ((string (gensym "WINCLASS-TEST-")))
    (is (not (equal 0 (register-class <class-name>))))))

(def-test |(register-class <exist-name>) should return zero to indicate error| ()
  (with-fixture class-name ((string (gensym "WINCLASS-TEST-")))
    (register-class <class-name>)
    (is (equal 0 (register-class <class-name>)))))

(def-test |(unregister-class <new-name>) should return nil to indicate error| ()
  (with-fixture class-name ((string (gensym "WINCLASS-TEST-")))
    (is (equal nil (unregister-class <class-name>)))))

(def-test |(unregister-class <exist-name>) should return t to indicate non errors| ()
  (with-fixture class-name ((string (gensym "WINCLASS-TEST-")))
    (register-class <class-name>)
    (is (equal t (unregister-class <class-name>)))))
