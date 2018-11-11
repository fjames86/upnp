;;;; Copyright (c) Frank James 2018 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :upnp
  :name "upnp"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "UPnP client"
  :license "MIT"
  :serial t
  :components
  ((:file "upnp"))
  :depends-on (:fsocket :http-parse :xmls :puri
	       #+(or win32 windows):winhttp #-(or win32 windows):drakma))
