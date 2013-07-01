;;;; deluge.lisp

(in-package #:deluge)

(defparameter *id* 0)

(defmacro with-deluge-json ((method) &body body)
  "Takes care of the boilerplate JSON of a deluge request"
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

(defmacro defdeluge (name method args &body json-builder)
  "Defines a function that returns the result of posting some
   deluge JSON"
  `(defun ,name ,args
     (deluge-result 
      (post-request *host* *port*
                    (with-deluge-json (,method)
                      ,@json-builder)))))

;;; *********************
;;; * auth functions
;;; *********************

(defdeluge login "auth.login" (pass)
  (yason:encode-array-element pass))

(defdeluge check-session "auth.check_session" ())

;;; *********************
;;; * web functions
;;; *********************

(defdeluge connect "web.connect" (host-id)
  (yason:encode-array-element host-id))

(defdeluge connected "web.connected" ())

(defdeluge get-hosts "web.get_hosts" ())

(defdeluge get-host-status "web.get_host_status" (host-id)
  (yason:encode-array-element host-id))

(defdeluge update-ui "web.update_ui" (state tracker &rest params)
  (yason:with-array ()
    (dolist (i params) (yason:encode-array-element i)))
  (yason:with-object ()
    (cond
      ((and state tracker) (error ":state and :tracker are mutually exclusive"))
      (state (yason:encode-object-element "state" state))
      (tracker (yason:encode-object-element "tracker_host" tracker)))))

(defdeluge get-torrent-info "web.get_torrent_info" (path)
  (yason:encode-array-element path))

(defdeluge get-torrent-status "web.get_torrent_status" (torrent-id &rest params)
  (yason:encode-array-element torrent-id)
  (yason:with-array ()
    (dolist (i params) (yason:encode-array-element i))))

(defdeluge add-torrent "web.add_torrents" (path options)
  (yason:with-array ()
    (yason:with-object ()
      (yason:encode-object-element "path" path)
      (yason:encode-object-element "options" options))))

;;; *********************
;;; * core functions
;;; *********************

(defdeluge pause-torrent "core.pause_torrent" (torrent-id)
  (yason:with-array ()
    (yason:encode-array-element torrent-id)))

(defdeluge resume-torrent "core.resume_torrent" (torrent-id)
  (yason:with-array ()
    (yason:encode-array-element torrent-id)))

(defdeluge remove-torrent "core.remove_torrent" (torrent-id &optional with-data)
  (yason:encode-array-element torrent-id)
  (yason:encode-array-element (or (and with-data 'yason:true) 'yason:false)))

(defdeluge get-config-values "core.get_config_values" (&rest params)
  (yason:with-array ()
    (dolist (i params) (yason:encode-array-element i))))
