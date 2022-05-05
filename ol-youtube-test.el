(require 'cort)
(require 'ol-youtube)

(cort-deftest-generate ol-youtube-test/-convert-time :=
	      '(((ol-youtube/-convert-time "1") 1)
		((ol-youtube/-convert-time "01") 1)
		((ol-youtube/-convert-time "0:01") 1)
		((ol-youtube/-convert-time "00:01") 1)
		((ol-youtube/-convert-time "0:00:01") 1)
		((ol-youtube/-convert-time "00:00:01") 1)
		((ol-youtube/-convert-time "1:00") 60)
		((ol-youtube/-convert-time "01:00") 60)
		((ol-youtube/-convert-time "0:01:00") 60)
		((ol-youtube/-convert-time "00:01:00") 60)
		((ol-youtube/-convert-time "1:00:00") 360)
		((ol-youtube/-convert-time "01:00:00") 360)
		))

(cort-deftest-generate ol-youtube-test/-create-complete-url :string=
		       '(((ol-youtube/-create-complete-url "1:00" "VIDEOID") "https://www.youtube.com/watch?v=VIDEOID&t=60")
		         ((ol-youtube/-create-complete-url "1:00" "VIDEOID" t) "https://www.youtube.com/watch?v=VIDEOID")
			 )
		       )
