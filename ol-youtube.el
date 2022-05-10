;;; ol-youtube.el --- Org custom link for YouTube videos -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Cj.bc-sd a.k.a Cj-bc

;; Author: Cj.bc-sd a.k.a Cj-bc <cj.bc-sd@outlook.jp>
;; Created: 29 Apr 2022
;; Keywords: multimedia, outlines
;; URL: https://github.com/Cj-bc/ol-youtube
;; Package-Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:
;; org hyperlink for Youtube video timestamps.
;; It provides link type that can specify time stamp of YouTube Video.
;; For details, please refer to README.org.

;;; Code:
(require 'ol)
(require 'org)
(require 'cl-macs)
(require 'parse-time)

(org-link-set-parameters "youtube"
			 :follow #'ol-youtube/follow
			 )

;;;; Variables
(defvar ol-youtube/-sessions (make-hash-table :test 'equal)
  "Hash table of currently running mpvs.
KEY is string and represents video-uri,
 and VALUE is mpv process object that is playing video-uri's video.
")

(defcustom ol-youtube/mpv-WM-title-template "ol-youtube mpv -- {}"
  "Template of mpv windows' title. {} wil be replaced with video-uri"
  )

;;;; --- Common utilities

(defun ol-youtube/-get-video-uri (&optional pom)
  "Get video uri from buffer.
As this process requires to access the buffer, this function
should not be called so much without care.
"
  (org-entry-get pom "OL_MPV_URI" t))

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




;;;; ol-youtube/-mpv

(defun ol-youtube/-mpv-WM-title (video-uri)
  "Return WM title of mpv for given video-uri"
  (string-replace "{}" video-uri ol-youtube/mpv-WM-title-template))

(defun ol-youtube/-mpv/terminate (video-uri)
  "Cleanup jobs after mpv"
  (let ((mpv-proc (gethash video-uri ol-youtube/-sessions)))
    (if mpv-proc
	(progn (delete-process mpv-proc)
	       (remhash video-uri ol-youtube/-sessions))
      (message (format "mpv isn't runnning for %s" video-uri))))
  )

(defun ol-youtube/-mpv/setup (video-uri)
  "Launch mpv for given video-uri
This spawns one process for mpv executable that fetch video and play.

This function pushe that process object to `ol-youtube/-sessions'.

Those processes will be killed when

+ buffer is killed
+ mpv is killed by user
"
  (unless (gethash video-uri ol-youtube/-sessions)
    (let ((mpv-proc (make-process
		      :name (format "ol-youtube mpv [%s]" video-uri)
		      :buffer nil
		      :sentinel 'ol-youtube/-mpv/sentinel
		      :connection-type 'pipe
		      :command `("mpv"
				 "--no-terminal"
				 ,(format "--title=%s" (ol-youtube/-mpv-WM-title video-uri))
				 "--no-input-terminal"
				 "--input-ipc-client=fd://0"
				 video-uri
				 ))))
      (process-put mpv-proc :video-uri video-uri)
      (puthash video-uri mpv-proc ol-youtube/-sessions)
      (add-hook 'kill-buffer-hook `(lambda ()
				     (ol-youtube/-mpv/terminate ,video-uri)) 0 t)
      )))

(defun ol-youtube/-mpv/sentinel (process event)
  "Cleanup processes when process event is occured.

Currently, any event will do cleanup. This shuold be
fixed, but I'm not sure which event I should waits for.
"
  (let ((video-uri (process-get process :video-uri)))
    (ol-youtube/-mpv/terminate video-uri)
    (message "ol-youtube: mpv for %s is closed" video-uri)))

(defun ol-youtube/-mpv/change-time (connection second)
  "Send JSON IPC through the CONNECTION to set
player head to SECOND.

CONNECTION is mpv process object, and second
is the time to set in integer.
" 
  (process-send-string
   connection
   (format "%s\n" (json-encode `(("command" . ["set_property" "time-pos" ,second]))))))

;;;; --- Follow function
(defun ol-youtube/follow (link arg)
  "Control associated mpv to jump to the timestamp.
Spawn mpv if it isn't spawned"
  (let ((video-uri (ol-youtube/-get-video-uri)))
    (unless (gethash video-uri ol-youtube/-sessions)
      (message "ol-mpv: Launching mpv for [%s]...This could take some time" video-uri)
      (ol-youtube/-mpv/setup video-uri))
    (let ((mpv-proc (gethash video-uri ol-youtube/-sessions)))
      (ol-youtube/-mpv/change-time mpv-proc (ol-youtube/-convert-time link))
    )))


(provide 'ol-youtube)

;;; ol-youtube.el ends here
