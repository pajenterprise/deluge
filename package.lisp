;;;; package.lisp

(defpackage #:deluge
  (:use #:cl)
  (:export #:set-host
           #:login
           #:check-session
           #:connect
           #:connected
           #:get-hosts
           #:get-host-status
           #:update-ui
           #:get-torrent-info
           #:get-torrent-status
           #:add-torrent
           #:pause-torrent
           #:resume-torrent
           #:remove-torrent
           #:get-config-values
           #:upload-torrent
           #:torrent+))

