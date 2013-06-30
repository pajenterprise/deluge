;;;; response.lisp

(in-package #:deluge)

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

