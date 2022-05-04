;;; Code:
(require 'ol)

(org-link-set-parameters "youtube"
			 :export #'ol-youtube/export
			 )

;;;; Variables
(defvar ol-youtube/-sessions (make-hash-table :test 'equal)
  "Hash table of currently running mpvs.
KEY is videoId, and VALUE is mpv process object that is playing
videoId's video.
")

(defcustom ol-youtube/mpv-WM-title-template "ol-youtube mpv -- {}"
  "Base path name for UNIX socket path. {} wil be replaced with videoId"
  )

;;;; --- Common utilities

(defun ol-youtube/-get-video-id (&optional pom)
  "Get video id from buffer.
As extraction requires to access the buffer, this function
should not be called so much without care.
"
  (org-entry-get pom "YOUTUBE_ID" t))

(defun ol-youtube/-get-link (videoId)
  "Create YouTube link without timestamp"
  (when videoId
    (format "https://www.youtube.com/watch?v=%s" videoId)
    ))

(defun ol-youtube/-convert-time (timestamp)
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



(defun ol-youtube/-create-complete-url (link videoId)
  "create complete URL from link content.
"
  (format "%s&t=%s" (ol-youtube/-get-link videoId) (ol-youtube/-convert-time link)))

;;;; --- Export function
(defun ol-youtube/export (link description format _)
  "Convert links into URL link"
  (let* ((videoId (ol-youtube/-get-video-id))
	 (url (ol-youtube/-create-complete-url link videoId))
  	 (desc (or description link))
  	 )
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" url desc))
      (`ascii (format "%s (%s)" desc url))
      (_ (format "%s (%s)" desc url)))))



;;;; ol-youtube/-mpv

(defun ol-youtube/-mpv-WM-title (videoId)
  "Return WM title of mpv for given videoId"
  (string-replace "{}" videoId ol-youtube/mpv-WM-title-template))

(defun ol-youtube/-mpv/terminate (videoId)
  "Do some work after mpv is down"
  (let ((mpv-proc (gethash videoId ol-youtube/-sessions)))
    (if mpv-proc
	(progn (delete-process mpv-proc)
	       (remhash videoId ol-youtube/-sessions))
      (message (format "mpv isn't runnning for %s" videoId))))
  )

(defun ol-youtube/-mpv/setup (videoId)
  "Launch mpv for given videoId
This spawns two processes:

1. mpv executable process that fetch video and play
2. Network process to access mpv IPC server

This function pushe those processes into
variable `ol-youtube/-sessions'.

Those processes will be killed when

+ buffer is killed
+ mpv is killed by user
"
  (unless (gethash videoId ol-youtube/-sessions)
    (let ((mpv-proc (make-process
		      :name (format "ol-youtube mpv [%s]" videoId)
		      :buffer nil
		      :sentinel 'ol-youtube/-mpv/sentinel
		      :connection-type 'pipe
		      :command `("mpv"
				 "--no-terminal"
				 ,(format "--title=%s" (ol-youtube/-mpv-WM-title videoId))
				 "--no-input-terminal"
				 "--input-ipc-client=fd://0"
				 ,(ol-youtube/-get-link videoId)
				 )
		      :plist `(:id ,videoId))))
      (puthash videoId mpv-proc ol-youtube/-sessions)
      (add-hook 'kill-buffer-hook `(lambda ()
				     (ol-youtube/-mpv/terminate ,videoId)) 0 t)
      )))

(defun ol-youtube/-mpv/ipc-buffer-name (videoId)
  "Return buffer name for IPC process for VIDEOID.

Whenever possible, you should get buffer from process object itself.
"
  (format "ol-youtube ipc server [%s]" videoId))

(defun ol-youtube/-mpv/sentinel (process event)
  "Cleanup processes when process event is occured.

Currently, any event will do cleanup. This shuold be
fixed, but I'm not sure which event I should waits for.
"
  (ol-youtube/-mpv/terminate (process-get process :id))
  )

(defun ol-youtube/-mpv/change-time (connection second)
  "Send JSON IPC through the CONNECTION to set
player head to SECOND.

CONNECTION is a network process that is connected to
UNIX socket for mpv's JSON IPC server
" 
  (process-send-string
   connection
   (format "%s\n" (json-encode `(("command" . ["set_property" "time-pos" ,second]))))))

;;;; --- Follow function
(defun ol-youtube/follow (link arg)
  "Control associated mpv to jump to the timestamp.
Spawn mpv if it isn't spawned"
  (let ((videoId (ol-youtube/-get-video-id)))
    (unless (gethash videoId ol-youtube/-sessions)
      (ol-youtube/-mpv/setup videoId))
    (let ((mpv-proc (gethash videoId ol-youtube/-sessions)))
      (ol-youtube/-mpv/change-time mpv-proc (ol-youtube/-convert-time link))
    )))


(provide 'ol-youtube)

;;; ol-youtube.el ends here
