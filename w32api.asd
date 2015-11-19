;;;; w32api.asd

(asdf:defsystem #:w32api
		:description "w32api is a set of cffi wrappers for Win32 API."
		:author "onixie <onixie@gmail.com>"
		:license "BSD"
		:serial t
		:in-order-to ((test-op (test-op "w32api/test")))
		:depends-on (#:cffi #:bordeaux-threads #:lparallel)
		:components ((:module "util"
				      :components ((:file "package")
						   (:file "cffi-extra")))
			     (:module "api"
				      :serial t
				      :components ((:file "type")
						   (:file "kernel32")
						   (:file "user32")
						   (:file "gdi32")))
			     (:file "package")
			     (:file "w32api")
			     ))

(asdf:defsystem #:w32api/test
		:depends-on (#:w32api #:fiveam #:lparallel)
		:perform (test-op (o s)
				  (let ((result (symbol-call '#:fiveam '#:run!
							     (find-symbol* '#:test '#:w32api/test))))
				    (symbol-call '#:fiveam '#:explain! result)
				    (unless (symbol-call 'fiveam 'results-status result)
				      (error "w32api/test failed."))))
		:components ((:module "test"
				      :serial t
				      :components ((:file "package")
						   (:file "w32api")))))
