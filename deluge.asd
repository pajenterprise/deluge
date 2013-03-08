;;;; deluge.asd

(asdf:defsystem #:deluge
  :serial t
  :description "Describe deluge here"
  :author "John Wood <j@jdtw.us>"
  :license "Specify license here"
  :depends-on (#:drakma
	       #:gzip-stream
	       #:yason
	       #:puri)
  :components ((:file "package")
               (:file "deluge")))

