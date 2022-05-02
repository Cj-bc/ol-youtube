;;; Code:
(require 'ol)

(org-link-set-parameters "youtube"
			 :export #'ol-youtube-export
			 )

;;;; Variables
(defvar ol-youtube--conns (make-hash-table :test 'equal)
  "List of currently active mpv connections.
each entry have three properties:

+ `:id'          :: Contain id of the movie
+ `:connection'  :: Contain connection to mpv UNIX socket
+ `:mpv-process' :: Contain process object of mpv

\(ol-youtube--conns '((:id \"fooBar\" :connection conn :mpv-process proc)))
")

(defcustom ol-youtube-socket-name-template "/tmp/ol-youtube-mpv-sock--{}"
  "path name template for UNIX socket path. {} wil be replaced with videoId"
  )

(defcustom ol-youtube-mpv-WM-title-template "ol-youtube mpv -- {}"
  "Base path name for UNIX socket path. {} wil be replaced with videoId"
  )

;;;; --- Common utilities

(defun ol-youtube--get-link (&optional pom)
  "Retrive YouTube link associated with entry at point-or-marker POM.
POM is the same as `org-entry-properties'.
"
  (let ((linkId (org-entry-get pom "YOUTUBE_ID" t)))
    (when (not (eq linkId nil))
      (format "https://www.youtube.com/watch?v=%s" linkId)
      )))

(defun ol-youtube--convert-time (timestamp)
  "Convert (HH:)MM:SS timestamp into seconds.
Return `nil' if conversion is failed.
"
  (let* ((strict (cl-case (length timestamp)
		  ;; SS, but first S is 0
		  (1 (format "00:00:0%s" timestamp))
		  ;; SS
		  (2 (format "00:00:%s" timestamp))
		  ;; MM:SS, but first M is 0
		  (4 (format "00:0%s" timestamp))
		  ;; MM:SS
		  (5 (format "00:%s" timestamp))
		  ;; HH:MM:SS, but first H is 0
		  (7 (format "0%s" timestamp))
		  ;; HH:MM:SS
		  (8 timestamp)
		  (t (error "Time stamp format is incorrect. Expected: [HH:][MM:]SS, but got %s" timestamp))))
	(parsed-time (parse-time-string strict)))
    (when parsed-time
      (+ (elt parsed-time 0)
	 (* (elt parsed-time 1) 60)
	 (* (elt parsed-time 2) 360)))))



(defun ol-youtube--create-complete-url (link)
  "create complete URL from link content.
"
  (format "%s&t=%s" (ol-youtube--get-link) (ol-youtube--convert-time link)))

;;;; --- Export function
(defun ol-youtube-export (link description format _)
  "Convert links into URL link"
  (let ((url (ol-youtube--create-complete-url link))
  	(desc (or description link))
  	)
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" url desc))
      (`ascii (format "%s (%s)" desc url))
      (_ (format "%s (%s)" desc url)))))

;;;; --- Follow function

(defun ol-youtube--socket-name-of (videoId)
  "Return UNIX socket path for videoId"
  (string-replace "{}" videoId ol-youtube-socket-name-template))

(defun ol-youtube--mpv-WM-title (videoId)
  "Return WM title of mpv for given videoId"
  (string-replace "{}" videoId ol-youtube-mpv-WM-title-template))

(defun ol-youtube--mpv/start (videoId link)
  "Create and return process object of mpv"
  (let ((proc (start-process (+ "ol-youtube mpv for " videoId)
			     nil
			     "mpv"
			     (ol-youtube--create-complete-url link)
			     "--title"
			     (ol-youtube--mpv-WM-title videoId)
			     "--input-ipc-server"
			     (ol-youtube--socket-name-of videoId)
			     ))
	(conn (make-network-process :remote (ol-youtube--socket-name-of videoId)
				    :buffer nil))
	)
    (puthash videoId (:process proc :connection conn))
    )
  )

(defun ol-youtube--mpv/terminate (videoId)
  "Do some work after mpv is down"
  (let ((conns (gethash videoId ol-youtube--conns)))
    (if conns
	(progn (delete-process (plist-get conns :connection))
	       (delete-process (plist-get conns :process))
	       (remhash videoId ol-youtube--conns))
      (message (format "mpv isn't runnning for %s" videoId))))
  )

(defun ol-youtube--mpv/setup (videoId)
  "Launch mpv for given videoId"
  (unless (gethash videoId ol-youtube--conns)
    (let* ((mpv-proc (make-process
		      :name (format "ol-youtube mpv [%s]" videoId)
		      :buffer nil
		      :sentinel 'ol-youtube--mpv/sentinel
		      :command `("mpv"
				 ,(format "--title=%s" (ol-youtube--mpv-WM-title videoId))
				 ,(format "--input-ipc-server=%s" (ol-youtube--socket-name-of videoId))
				 )
		      :plist `(:id ,videoId)))
	   (conn (make-network-process :name (format "ol-youtube connection [%s]" videoId)
				       :sentinel 'ol-youtube--mpv/sentinel
				       :plist `(:id ,videoId)
				       :remote (ol-youtube--socket-name-of videoId))))
      (puthash videoId `(:connection ,conn :process ,mpv-proc) ol-youtube--conns)
      (add-hook 'kill-buffer-hook `(lambda ()
				     (ol-youtube--mpv/terminate ,videoId)) 0 t)
      )))

(defun ol-youtube--mpv/sentinel (process event)
  "Cleanup processes when process event is occured.

Currently, any event will do cleanup. This shuold be
fixed, but I'm not sure which event I should waits for.
"
  (ol-youtube--mpv/terminate (process-get process :id))
  )

(defun ol-youtube-follow (link arg)
  "Control associated mpv to jump to the timestamp.
Spawn mpv if it isn't spawned"
  (unless (gethash (org-entry-get nil "YOUTUBE_ID" t) ol-youtube--conns)
    (ol-youtube--mpv/setup (org-entry-get nil "YOUTUBE_ID" t)))
  (ol-youtube--mpv-api/)
  )


(provide 'ol-youtube)

;;; ol-youtube.el ends here
