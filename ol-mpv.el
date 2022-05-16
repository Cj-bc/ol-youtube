;;; ol-mpv.el --- Org custom link for YouTube videos -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Cj.bc-sd a.k.a Cj-bc

;; Author: Cj.bc-sd a.k.a Cj-bc <cj.bc-sd@outlook.jp>
;; Created: 29 Apr 2022
;; Keywords: multimedia, outlines
;; URL: https://github.com/Cj-bc/ol-mpv
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

(org-link-set-parameters "mpv"
			 :follow #'ol-mpv/follow
			 )

;;;; Variables
(defvar ol-mpv/sessions (make-hash-table :test 'equal)
  "Hash table of currently running mpvs.
KEY is string and represents video-uri,
 and VALUE is mpv process object that is playing video-uri's video.
")

(defgroup ol-mpv '((ol-mpv/mpv-WM-title-template custom-variable))
  "Group for ol-mpv library"
  :group 'external
  :group 'org
  )
(defcustom ol-mpv/mpv-WM-title-template "ol-mpv mpv -- {}"
  "Template of mpv windows' title. {} wil be replaced with video-uri"
  :type (string)
  )


;; Symbol that indicates URI type.
;;;; --- Common utilities

(defun ol-mpv/get-video-uri (&optional pom)
  "Get video uri from buffer.
As this process requires to access the buffer, this function
should not be called so much without care.
"
  (org-entry-get pom "OL_MPV_URI" t))

(defun ol-mpv/convert-time (timestamp)
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




;;;; ol-mpv/mpv

(defun ol-mpv/mpv-WM-title (video-uri)
  "Return WM title of mpv for given video-uri"
  (string-replace "{}" video-uri ol-mpv/mpv-WM-title-template))

(defun ol-mpv/mpv/terminate (video-uri)
  "Cleanup jobs after mpv"
  (let ((mpv-proc (gethash video-uri ol-mpv/sessions)))
    (if mpv-proc
	(progn (delete-process mpv-proc)
	       (remhash video-uri ol-mpv/sessions))
      (message (format "mpv isn't runnning for %s" video-uri))))
  )

(defun ol-mpv/mpv/setup (video-uri &optional initial-pos)
  "Launch mpv for given video-uri
This spawns one process for mpv executable that fetch video and play.

This function pushe that process object to `ol-mpv/sessions'.

Those processes will be killed when

+ buffer is killed
+ mpv is killed by user
"
  (unless (gethash video-uri ol-mpv/sessions)
    (let ((mpv-proc (make-process
		      :name (format "ol-mpv mpv [%s]" video-uri)
		      :buffer nil
		      :sentinel 'ol-mpv/mpv/sentinel
		      :connection-type 'pipe
		      :command `("mpv"
				 "--no-terminal"
				 ,(format "--title=%s" (ol-mpv/mpv-WM-title video-uri))
				 ,(format "--start=%s" (or initial-pos 0))
				 "--no-input-terminal"
				 "--input-ipc-client=fd://0"
				 ,video-uri
				 ))))
      (process-put mpv-proc :video-uri video-uri)
      (puthash video-uri mpv-proc ol-mpv/sessions)
      (add-hook 'kill-buffer-hook `(lambda ()
				     (ol-mpv/mpv/terminate ,video-uri)) 0 t)
      )))

(defun ol-mpv/mpv/sentinel (process _)
  "Cleanup processes when process event is occured.

Currently, any event will do cleanup. This shuold be
fixed, but I'm not sure which event I should waits for.
"
  (let ((video-uri (process-get process :video-uri)))
    (ol-mpv/mpv/terminate video-uri)
    (message "ol-mpv: mpv for %s is closed" video-uri)))

(defun ol-mpv/mpv/change-time (connection second)
  "Send JSON IPC through the CONNECTION to set
player head to SECOND.

CONNECTION is mpv process object, and second
is the time to set in integer.
" 
  (process-send-string
   connection
   (format "%s\n" (json-encode `(("command" . ["set_property" "time-pos" ,second]))))))


;;;; --- ol-mpv/uri
(defun ol-mpv/uri/validate (uri)
  "Return validated URI or throw error signal.

This only ensure that:
+ it is non-nil
+ if it's local file path, it exists

Possible errors:
+ `ol-mpv/uri/not-found-error', when given URI is `nil'
+ `ol-mpv/uri/unreachable', when given URI is local filepath & not found
+ Raw error when something wrong happend
"
  (unless uri
    (signal 'ol-mpv/uri/not-found-error nil))
  (pcase (ol-mpv/uri/get-type uri)
    ('filepath (if (file-exists-p uri)
		  (expand-file-name uri)
		(signal 'ol-mpv/uri/unreachable nil)))
    ('protocol uri)
    (_ (error "ol-mpv: unknown error occured"))
    ))

(defun ol-mpv/uri/get-type (uri)
  "Return symbol of URI type

Possible symbols are:

+ 'protocol for URI that start with \"<protocol>://\"
+ 'filepath for other path
+ nil for nil
"
  (when uri
      (if (string-match-p "://" uri)
	  'protocol
	'filepath
	  )))

;;;; --- Errors
(define-error 'ol-mpv-error "ol-mpv: Generic error for ol-mpv packge")
(define-error 'ol-mpv/uri/not-found-error "ol-mpv: Couldn't find URI" 'ol-mpv-error)
(define-error 'ol-mpv/uri/unreachable "ol-mpv: Couldn't reach URI" 'ol-mpv-error)

;;;; --- Follow function
(defun ol-mpv/follow (link _)
  "Control associated mpv to jump to the timestamp.
Spawn mpv if it isn't spawned"
  (let ((video-uri (ol-mpv/uri/validate (ol-mpv/get-video-uri))))
    (unless (gethash video-uri ol-mpv/sessions)
      (message "ol-mpv: Launching mpv for [%s]...This could take some time" video-uri)
      (ol-mpv/mpv/setup video-uri (ol-mpv/convert-time link)))
    (let ((mpv-proc (gethash video-uri ol-mpv/sessions)))
      (ol-mpv/mpv/change-time mpv-proc (ol-mpv/convert-time link))
    )))


(provide 'ol-mpv)

;;; ol-mpv.el ends here
