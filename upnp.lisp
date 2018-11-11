;;;; Copyright (c) Frank James 2018 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;;
;;; This file defines a basic Universal Plug and Play (UPnP)
;;; protocol client. This includes the SSDP protocol for discovery
;;; and SOAP for actions/RPCs. 
;;; 

(defpackage #:upnp
  (:use #:cl)
  (:export #:discover
	   #:get-service-list
	   #:get-service-actions 
	   #:call-service-action
	   #:*timeout*
	   #:defaction))

(in-package #:upnp)

(defvar *mcast* #(239 255 255 250))
(defvar *port* 1900)
(defvar *addr* (fsocket:sockaddr-in *mcast* *port*))
(defvar *timeout* 5000)

(defun m-search ()
  (with-output-to-string (s)
    (format s "M-SEARCH * HTTP/1.1~C~C" #\return #\newline)
    (format s "HOST: ~A~C~C" (fsocket:sockaddr-string *addr*) #\return #\newline)
    (format s "ST: upnp:rootdevice~C~C" #\return #\newline)
    (format s "MX: 2~C~C" #\return #\newline)
    (format s "MAN: \"ssdp:discover\"~C~C" #\return #\newline)
    (format s "~C~C" #\return #\newline)))

(defun parse-http (buf)
  (funcall (http-parse:make-parser (make-instance 'http-parse:http-response))
	   buf))

(defun parse-ssdp (buf)
  (do ((headers (http-parse:http-headers (parse-http buf)) (cddr headers))
       (ssdp nil))
      ((null headers) ssdp)
    (let ((key (car headers))
	  (val (cadr headers)))
      (case key
	((:|EXT:| :EXT) (setf (getf ssdp :ext) val))
	(:LOCATION (setf (getf ssdp :location) val))
	(:SERVER (setf (getf ssdp :server) val))
	(:ST (setf (getf ssdp :st) val))
	(:USN (setf (getf ssdp :usn) val))))))

(defun discover (&optional (timeout *timeout*))
  "Performs an SSDP discovery multicast. Returns a list of discovered devices.
Pass one of these in to GET-SERVICE-LIST to query the device for its service list.
TIMEOUT ::= milliseconds to wait for responses. Default is 5000ms.
"
  (let ((buf (make-array (* 32 1024) :element-type '(unsigned-byte 8))))
    (fsocket:with-udp-socket (s)
      (setf (fsocket:socket-option s :socket :rcvtimeo) 2000)
      (setf (fsocket:socket-option s :ip :ip-multicast-ttl) 2)
      (fsocket:socket-sendto s
			     (babel:string-to-octets (m-search))
			     (fsocket:sockaddr-in *mcast* *port*))
      ;; await replies
      (fsocket:with-poll (pc)
	(fsocket:poll-register pc
			       (make-instance 'fsocket:pollfd
					      :fd s
					      :events (fsocket:poll-events :pollin)))
	(do ((done nil)
	     (resps nil))
	    (done resps)
	  (cond
	    ((fsocket:poll pc :timeout (or timeout 5000))
	     (multiple-value-bind (n raddr) (fsocket:socket-recvfrom s buf)
	       (declare (ignore raddr))
	       (push (let ((ssdp (parse-ssdp (subseq buf 0 n))))
		       #+nil(setf (getf ssdp :raddr) raddr)
		       ssdp)
		     resps)))
	    (t (setf done t))))))))



(defun http-request (url &key method post-data headers)
  #+(or win32 windows)(winhttp:http-request url
					    :method (or method :get)
					    :post-data post-data
					    :headers headers
					    :timeout *timeout*)
  #-(or win32 windows)(drakma:http-request url
					   :method (or method :get)
					   :content post-data
					   :additional-headers headers
					   :connection-timeout (/ *timeout* 1000)))



(defun get-service-list (ssdp)
  "Query a device for its service list. Returns a list of SCPD structures, that can be passed in to GET-SERVICE-ACTIONS.
SSDP ::= structure returned from DISCOVER.
"
  (let* ((location (getf ssdp :location))
	 (nodes (xmls:parse (http-request location)))
	 (ret nil))
    (dolist (node (cddr nodes))
      (let ((nname (car (first node)))
	    (cnodes (cddr node)))
      (cond
	((string-equal nname "device")
	 (dolist (dn cnodes)
	   (cond
	     ((string-equal (car (first dn)) "serviceList")
	      (dolist (service-node (cddr dn))
		(let ((service (list :location location)))
		  (dolist (snode (cddr service-node))
		    (cond
		      ((string-equal (car (first snode)) "serviceType")
		       (setf (getf service :service-type) (third snode)))
		      ((string-equal (car (first snode)) "serviceId")
		       (setf (getf service :service-id) (third snode)))
		      ((string-equal (car (first snode)) "SCPDURL")
		       (setf (getf service :scpd-url) (third snode)))
		      ((string-equal (car (first snode)) "controlURL")
		       (setf (getf service :control-url) (third snode)))
		      ((string-equal (car (first snode)) "eventSubURL")
		       (setf (getf service :event-sub-url) (third snode)))))
		  (push service ret))))))))))
    ret))

	 
      


(defun upnp-url (location path)
  (let ((uri (puri:uri location)))
    (setf (puri:uri-path uri)
	  (if (char= (char path 0) #\/)
	      path
	      (format nil "/~A" path)))
    (with-output-to-string (s)
      (puri:render-uri uri s))))

(defun get-service-actions (service-info)
  "Get list of actions supported by the device. 
Returns (values service-actions state-variables).

SERVICE-INFO ::= structure returned from GET-SERVICE-LIST.
"
  (let ((nodes (xmls:parse
		(http-request
		 (upnp-url (getf service-info :location) (getf service-info :scpd-url)))))
	(state-variables nil)
	(actions nil))
    (unless (string= (car (first nodes)) "scpd")
      (error "Expected scpd element, got ~S~%" (car (first nodes))))
    (dolist (node (cddr nodes))
      (let ((name (car (first node))))
	(cond
	  ((string= name "serviceStateTable")
	   ;; state varibles
	   (dolist (sv (cddr node))
	     (when (string-equal (car (first sv)) "stateVariable")
	       (let ((scpd-sv nil))
		 (dolist (svnode (cddr sv))
		   (cond
		     ((string-equal (car (first svnode)) "name")
		      (setf (getf scpd-sv :name) (third svnode)))
		     ((string-equal (car (first svnode)) "dataType")
		      (setf (getf scpd-sv :data-type) (third svnode)))))
		 (push scpd-sv state-variables)))))
	  ((string= name "actionList")
	   ;; rpc list
	   (dolist (al (cddr node))
	     (when (string-equal (car (first al)) "action")
	       (let* ((name (third (third al)))
		      (arglist (cddr (fourth al)))
		      (scpd-action (list :name name :arguments nil)))
		 (dolist (arg arglist)
		   (let ((scpd-arg nil))
		     (dolist (argn (cddr arg))
		       (cond
			 ((string-equal (car (first argn)) "name")
			  (setf (getf scpd-arg :name) (third argn)))
			 ((string-equal (car (first argn)) "direction")
			  (setf (getf scpd-arg :outp)
				(string-equal (third argn) "out")))
			 ((string-equal (car (first argn)) "releatedStateVariable")
			  (setf (getf scpd-arg :state-variable) (third argn)))))
		     (push scpd-arg (getf scpd-action :arguments))))
		 (push scpd-action actions))))))))
  (values actions state-variables)))

(defun gen-action-message (action-name service-type args)
  (with-output-to-string (s)
    (format s "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">~%")
    (format s "<s:Body>~%")
    (format s "<u:~A xmlns:u:~S>~%" action-name service-type)
    (dolist (arg args)
      (destructuring-bind (arg-name arg-val) arg
	(format s "<~A>~A</~A>~%" arg-name arg-val arg-name)))
    (format s "</u:~A>~%" action-name)
    (format s "</s:Body>~%")
    (format s "</s:Envelope>~%")))


(define-condition upnp-error (error)
  ((description :initform nil :initarg :description :reader upnp-error-desc))
  (:report (lambda (c stream)
	     (format stream "UPNP Fault")
	     (let ((d (upnp-error-desc c)))
	       (when d
		 (format stream ": ~A" d))))))

(defun parse-action-response (xml)
  (let ((nodes (xmls:parse xml)))
    (dolist (node (cddr nodes))
      (when (string-equal (car (first node)) "Body")
	(cond
	  ((string-equal (car (first (third node))) "Fault")
	   (let ((desc nil))
	     (dolist (node (cddr (third node)))
	       (cond
		 ((string-equal (car node) "detail")
		  (dolist (node (cddr (third node)))
		    (cond
		      ((string-equal (car (first node)) "errorDescription")
		       (setf desc (third node))))))))
	     (error 'upnp-error :description desc)))
	  (t
	   (return-from parse-action-response
	     (mapcar (lambda (x)
		       (list (first x) (third x)))
		     (cddr (third node))))))))))
		   

(defun %call-service-action (control-url action-name service-type args)
  (let ((msg (gen-action-message action-name service-type args)))
    (parse-action-response
     (http-request control-url
		   :method :post
		   :post-data msg
		   :headers (list (list "CONTENT-TYPE" "text/xml")
				  (list "SOAPACTION"
					(format nil "~A#~A" service-type action-name)))))))

(defun call-service-action (service-info action-name args)
  "Call a service action (control message). 
SERVICE-INFO ::= structure returned from GET-SERVICE-LIST 
ACTION-NAME ::= name of action to invoke, from GET-SERVICE-ACTIONS
ARGS ::= list of in-args in form (arg-name arg-val). Get action args from GET-SERVICE-ACTIONS. 
"
  (let ((control-url (upnp-url (getf service-info :location)
			       (getf service-info :control-url)))
	(service-type (getf service-info :service-type)))
    (%call-service-action control-url action-name service-type args)))
  
(defmacro defaction (name (action-name control-url service-type) &rest args)
  "Define action client wrapper function." 
  `(defun ,name (url) ,@(mapcar #'car args)
	  (%call-service-action (upnp-url url ,control-url)
				,action-name ,service-type
				(list ,@(mapcar (lambda (arg)
						  (destructuring-bind (arg-sym arg-name) arg
						    `(list ,arg-name ,arg-sym)))
						args)))))

