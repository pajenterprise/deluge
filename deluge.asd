;;;; deluge.asd

(asdf:defsystem #:deluge
  :serial t
  :description "CL interface to the deluge torrent client webui"
  :author "John Wood <j@jdtw.us>"
  :license "Specify license here"
  :depends-on (#:drakma
	       #:gzip-stream
	       #:yason
	       #:puri)
  :components ((:file "package")
               (:file "response")
               (:file "request")
               (:file "util")
               (:file "deluge")
               (:file "client")))

