
UPnP
-----

Common Lisp implementation of a UPnP client.

1. Usage
---------

 * discover services by multicasting to 239.255.255.250:1900
 
 ```
 (discover)
 ->
 ((:LOCATION "http://192.168.0.1:5431/dyndev/uuid:90210654-2d80-802d-5406-21902154800000" :EXT NIL :SERVER "Custom/1.0 UPnP/1.0 Proc/Ver"))
 ```

 * This returns a list of the services that responded

 * Choose a service you want to use. Get its LOCATION url.

 * Ask the service for its service list using `GET-SERVICE-LIST` 
 ```
 (get-service-list (first *))
 ->
 ((:SCPD-URL "/dynsvc/Layer3Forwarding:1.xml" :EVENT-SUB-URL
  "/uuid:90210654-2d80-802d-5406-21902154800000/Layer3Forwarding:1"
  :CONTROL-URL
  "/uuid:90210654-2d80-802d-5406-21902154800000/Layer3Forwarding:1" :SERVICE-ID
  "urn:upnp-org:serviceId:Layer3Forwarding.1" :SERVICE-TYPE
  "urn:schemas-upnp-org:service:Layer3Forwarding:1"))
 ```

 * Get the actions that the service offers by calling `GET-SERVICE-ACTIONS` with
 the SCPD url
 ```
 (defvar *sinfo* (first *))
 (upnp:get-service-actions *sinfo*)
 ->
 ((:NAME "GetRDM" :ARGUMENTS ((:OUTP T :NAME "RDMValue"))))
 ((:DATA-TYPE "string" :NAME "DefaultConnectionService"))
 ```

 * This returns two values: the avilable actions and the service state variables.

 * Call an action using `CALL-SERVICE-ACTION`
 ```
 (upnp:call-service-action *sinfo* "GetDefaultConnectionService" nil)
 ```

 * Define convenience function
 ```
 (upnp:defaction get-rdm ("GetRDM" #.(getf *sinfo* :control-url) #.(getf *sinfo* :service-type)))

 ;; call it 
 (get-rdm "http://192.168.0.14:1400")
 -> (("RDMValue" "0"))
 ```

2. Dependencies
----------------

 * fsocket : for UDP multicast
 * drakma, winhttp, http-parse, puri, xmls : for http/xml things 
 
Frank James
2018