;;;; deluge.lisp

(in-package #:deluge)

;;; "deluge" goes here. Hacks and glory await!

(setf drakma:*header-stream* *standard-output*)

(defparameter *host* "localhost")
(defparameter *port* 8112)

(defun set-host (host &optional (port 8112))
  (setf *host* host *port* port))

(defclass response ()
  ((json :accessor response-json :initarg :json)
   (status :accessor response-status :initarg :status)
   (headers :accessor response-headers :initarg :headers)))

(defmethod print-object ((obj response) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (gethash "result" (response-json obj)))))

(defmethod success-p ((resp response))
  (= (response-status resp) 200))

(defmethod deluge-result ((obj response))
  (gethash "result" (response-json obj) nil))

(defmethod deluge-success-p ((obj response))
  (not (gethash "error" (response-json obj) t)))

(defun unzip-response (response)
  (coerce (map 'list #'code-char (gzip-stream:gunzip-sequence response)) 'string))

(let ((id 0))
  (defun build-json (method params)
    (let ((json-string
	   (yason:with-output-to-string* ()
	     (yason:with-object ()
	       (yason:encode-object-element "method" method)
	       (yason:with-object-element ("params")
		 (yason:with-array ()
		   (dolist (i params) (yason:encode-array-element i))))
	       (yason:encode-object-element "id" (incf id))))))
      (format t "~a~%" json-string)
      json-string))
  (defun reset-id () (setf id 0)))

(defun make-deluge-uri (host port path)
  (make-instance 'puri:uri :host (or (puri:uri-host (puri:uri host)) (puri:uri-path (puri:uri host)))
		 :port port :path path :scheme :http))

(let ((cookie-jar (make-instance 'drakma:cookie-jar)))
  (defun post-request (host port json)
    (multiple-value-bind (body status headers)
	(drakma:http-request (make-deluge-uri host port "/json")
			     :method :post
			     :content-type "application/json"
			     :content json
			     :cookie-jar cookie-jar)
      (let ((json (if (string= (cdr (assoc :content-encoding headers)) "gzip")
		      (unzip-response body)
		      body)))
	(format t "~s~%" json)
	(make-instance 'response :json (yason:parse json) :status status :headers headers)))))

(defmacro defdeluge (name method args)
  `(defun ,name (,@args)
     (deluge-result 
      (post-request *host* *port* (build-json ,method (list ,@args))))))

(defdeluge login "auth.login" (pass))
(defdeluge check-session "auth.check_session" ())
(defdeluge connected "web.connected" ())
(defdeluge get-hosts "web.get_hosts" ())
(defdeluge get-host-status "web.get_host_status" (host-id))
(defdeluge connect "web.connect" (host-id))

(defdeluge update-ui-internal "web.update_ui" (props foo))

(defun update-ui (params)
  (update-ui-internal 
   (yason:with-output-to-string* ()
     (yason:with-array ()
       (dolist (i params) (yason:encode-array-element i))))
   (yason:with-output-to-string* ()
     (yason:with-object ()))))
