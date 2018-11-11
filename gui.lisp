;;;; Copyright (c) Frank James 2018 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


;;; This file defines a simple Win32 GUI for UPnP device
;;; discovery. It could be extended to support listing services
;;; and their actions, also possibly invoking actions.

(defpackage #:upnp/gui
  (:use #:cl #:upnp)
  (:export #:gui))

(in-package #:upnp/gui)

(defun upnp-create (hwnd)
  (ftw:add-menu-bar hwnd
		    '((:menu (:popup) :name "&File"
		       :children
		       ((:item (:string)
			       :name "&Quit"
			       :id 2)))))
  (let ((h (ftw:create-button "Discover"
			      :parent hwnd
			      :styles (logior ftw::+ws-visible+
					      ftw::+ws-child+
					      ftw::+bs-defpushbutton+)
			      :x 25
			      :y 25
			      :width 75
			      :height 25
			      :menu 1)))
    (ftw:register-hwnd 'btn-discover h 1))
  (let ((h (ftw:create-window ftw::+wc-listview+
			      :styles (logior ftw::+ws-visible+
					      ftw::+ws-child+
					      ftw::+lvs-report+)
			      :x 25 :y 60
			      :width 300 :height 250
			      :parent hwnd)))
    (ftw:send-message h ftw::+lvm-setextendedlistviewstyle+
		      0 ftw::+lvs-ex-fullrowselect+)
    (ftw:listview-insert-column h "Server" 0 :width 200)
    (ftw:listview-insert-column h "Location" 1 :width 200)
    (ftw:register-hwnd 'lv-devices h)))

(defun upnp-size (hwnd w h)
  (declare (ignore hwnd w))
  (let ((h2 (ftw:hwnd-by-name 'lv-devices)))
    (when h2
      (ftw:set-window-pos h2 :top 25 60 450 (- h 100)))))

(defun upnp-command (hwnd code)
  (ftw:switch code
    (1 ;; discover
     (let ((devices (upnp:discover 2000))
	   (h (ftw:hwnd-by-name 'lv-devices)))
       (ftw:send-message h ftw::+lvm-deleteallitems+ 0 0)
       (dolist (device devices)
	 (let ((idx (ftw:listview-insert-item h (getf device :server) 0)))
	   (ftw:listview-set-item h idx (getf device :location) 1)))))
    (2 ;; quit
     (ftw:send-message hwnd ftw::+wm-close+ 0 0))))

(ftw:defwndproc upnp-wndproc (hwnd msg wparam lparam)
  (ftw:switch msg
    (ftw::+wm-create+ (upnp-create hwnd))
    (ftw::+wm-size+ (upnp-size hwnd (ftw:loword lparam) (ftw:hiword lparam)))
    (ftw::+wm-command+ (upnp-command hwnd (ftw:loword wparam)))
    (ftw::+wm-destroy+ (ftw:post-quit-message))
    )
  (ftw:default-window-proc hwnd msg wparam lparam))


(defun gui ()
  (ftw:default-message-loop 'upnp-wndproc
      :class-name "UPNP_MAIN"
      :title "UPnP Client"
      :width 500
      :height 500))

