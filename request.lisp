;;;; request.lisp

(in-package #:deluge)

;; for debugging:
(setf drakma:*header-stream* *standard-output*)

(defparameter *host* "localhost")
(defparameter *port* 8112)

(defun set-host (host &optional (port 8112))
  (setf *host* host *port* port))

(defun make-deluge-uri (host port path)
  (make-instance 'puri:uri
                 :host (or (puri:uri-host (puri:uri host))
                           (puri:uri-path (puri:uri host)))
		 :port port
                 :path path
                 :scheme :http))

(defun unzip-response (response)
  (coerce
   (map 'list #'code-char
        (gzip-stream:gunzip-sequence response)) 'string))

(defmacro defrequest (name (path obj content) &body args)
  `(defun ,name (host port ,content)
     (multiple-value-bind (body status headers)
         (drakma:http-request (make-deluge-uri host port ,path)
                              :method :post
                              ;; :proxy '("localhost" 8888) ;; fiddler
                              :keep-alive t
                              :close nil
                              ,@args)
       (let ((json (if (string= (cdr (assoc :content-encoding headers)) "gzip")
                       (unzip-response body)
                       body)))
         (format t "~s~%" json)
         (make-instance ',obj
                        :json (and (= status 200) (yason:parse json))
                        :status status
                        :headers headers)))))

(let ((cookie-jar (make-instance 'drakma:cookie-jar)))
  (defrequest post-request ("/json" response json)
    :content-type "application/json"
    :content json
    :cookie-jar cookie-jar)
  (defrequest upload-torrent ("/upload" upload-response file)
    :form-data t
    :parameters
    (list
     (cons :|file| ;; must be |file| so that it's printed as lowercase
           (list file
                 :content-type "application/x-bittorrent"
                 :filename (file-namestring file))))
    :cookie-jar cookie-jar))
