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

(defclass upload-response (response) ())

(defmethod print-object ((obj response) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (deluge-result obj))))

(defmethod success-p ((resp response))
  (= (response-status resp) 200))

(defmethod deluge-result ((obj upload-response))
  (gethash "files" (response-json obj) nil))

(defmethod deluge-result ((obj response))
  (gethash "result" (response-json obj) nil))

(defmethod deluge-success-p ((obj upload-response))
  (gethash "success" (response-json obj) nil))

(defmethod deluge-success-p ((obj response))
  (not (gethash "error" (response-json obj) t)))

(defun unzip-response (response)
  (coerce (map 'list #'code-char (gzip-stream:gunzip-sequence response)) 'string))

(defparameter *id* 0)

(defmacro with-deluge-json ((method) &body body)
  `(let ((json-string
          (yason:with-output-to-string* ()
            (yason:with-object ()
              (yason:encode-object-element "method" ,method)
              (yason:with-object-element ("params")
                (yason:with-array ()
                  ,@body))
              (yason:encode-object-element "id" (incf *id*))))))
     (format t "~a~%" json-string)
     json-string))

(defun make-deluge-uri (host port path)
  (make-instance 'puri:uri :host (or (puri:uri-host (puri:uri host)) (puri:uri-path (puri:uri host)))
		 :port port :path path :scheme :http))

(defmacro defrequest (name (path obj content) &body args)
  `(defun ,name (host port ,content)
     (multiple-value-bind (body status headers)
         (drakma:http-request (make-deluge-uri host port ,path)
                              :method :post
                              :proxy '("localhost" 8888) ;; fiddler
                              :keep-alive t
                              :close nil
                              ,@args)
       (let ((json (if (string= (cdr (assoc :content-encoding headers)) "gzip")
                       (unzip-response body)
                       body)))
         (format t "~s~%" json)
         (make-instance ',obj :json (yason:parse json) :status status :headers headers)))))

(let ((cookie-jar (make-instance 'drakma:cookie-jar)))
  (defrequest post-request ("/json" response json)
    :content-type "application/json"
    :content json
    :cookie-jar cookie-jar)
  (defrequest upload-torrent ("/upload" upload-response file)
    :form-data t
    :parameters
    (list
     (cons '|file| ;; must be |file| so that it's printed as lowercase
           (list file
                 :content-type "application/x-bittorrent"
                 :filename (file-namestring file))))
    :cookie-jar cookie-jar))

(defmacro defdeluge (name method args &body json-builder)
  `(defun ,name ,args
     (deluge-result 
      (post-request *host* *port*
                    (with-deluge-json (,method)
                      ,@json-builder)))))

(defdeluge login "auth.login" (pass)
  (yason:encode-array-element pass))

(defdeluge check-session "auth.check_session" ())

(defdeluge connect "web.connect" (host-id)
  (yason:encode-array-element host-id))

(defdeluge connected "web.connected" ())

(defdeluge get-hosts "web.get_hosts" ())

(defdeluge get-host-status "web.get_host_status" (host-id)
  (yason:encode-array-element host-id))

(defdeluge update-ui "web.update_ui" (params &key state tracker)
  (yason:with-array ()
    (dolist (i params) (yason:encode-array-element i)))
  (yason:with-object ()
    (when state
      (yason:encode-object-element "state" state))
    (when tracker
      (yason:encode-object-element "tracker_host" tracker))))

(defdeluge get-torrent-status "web.get_torrent_status" (torrent-id params)
  (yason:encode-array-element torrent-id)
  (yason:with-array ()
    (dolist (i params) (yason:encode-array-element i))))

(defdeluge pause-torrent "core.pause_torrent" (torrent-id)
  (yason:with-array ()
    (yason:encode-array-element torrent-id)))

(defdeluge resume-torrent "core.resume_torrent" (torrent-id)
  (yason:with-array ()
    (yason:encode-array-element torrent-id)))
