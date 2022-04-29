(require 'ol)

(org-link-set-parameters "youtube"
			 :export #'ol-youtube-export
			 )

(defun ol-youtube--get-link (&optional pom)
  "Retrive YouTube link that is associated with
entry at point-or-marker pom.
POM is the same as `org-entry-properties'.
"
  (let ((linkId (org-entry-get pom "YOUTUBE_ID" t)))
    (when (not (eq linkId nil))
      (format "https://www.youtube.com/watch?v=%s" linkId)
      )))

(defun ol-youtube--convert-time (timestamp)
  "Convert (HH:)MM:SS timestamp into seconds.
Returns `nil' if conversion is failed.
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

(defun ol-youtube-export (link description format _)
  "Convert links into URL link"
  (let ((url (ol-youtube--create-complete-url link))
  	(desc (or description link))
  	)
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" url desc))
      (`ascii (format "%s (%s)" desc url))
      (_ (format "%s (%s)" desc url)))))

(provide 'ol-youtube)
