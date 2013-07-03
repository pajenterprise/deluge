;;;; deluge.asd

(asdf:defsystem #:deluge
  :serial t
  :description "CL interface to the Deluge torrent client WebUI"
  :author "John Wood <j@jdtw.us>"
  :license "Simplified BSD"
  :depends-on (#:drakma
	       #:gzip-stream
	       #:yason
	       #:puri)
  :components ((:file "package")
               (:file "response")
               (:file "request")
               (:file "deluge")))

