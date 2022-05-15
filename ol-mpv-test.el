(require 'cort)
(require 'ol-mpv)

;; Test for ol-mpv/convert-time
(cort-deftest-generate ol-mpv-test/-convert-time :=
	      '(((ol-mpv/convert-time "1") 1)
		((ol-mpv/convert-time "01") 1)
		((ol-mpv/convert-time "0:01") 1)
		((ol-mpv/convert-time "00:01") 1)
		((ol-mpv/convert-time "0:00:01") 1)
		((ol-mpv/convert-time "00:00:01") 1)
		((ol-mpv/convert-time "1:00") 60)
		((ol-mpv/convert-time "01:00") 60)
		((ol-mpv/convert-time "0:01:00") 60)
		((ol-mpv/convert-time "00:01:00") 60)
		((ol-mpv/convert-time "1:00:00") 360)
		((ol-mpv/convert-time "01:00:00") 360)
		))

;; Test for ol-mpv/uri/get-type
(cort-deftest-generate ol-mpv-test/uri/get-type :eq
		       '(((ol-mpv/uri/get-type nil) nil)
			 ((ol-mpv/uri/get-type "some-file.mp4") 'filepath)
			 ((ol-mpv/uri/get-type "./some-file.mp4") 'filepath)
			 ((ol-mpv/uri/get-type "../some-file.mp4") 'filepath)
			 ((ol-mpv/uri/get-type "~/some-file.mp4") 'filepath)
			 ((ol-mpv/uri/get-type "/some-file.mp4") 'filepath)
			 ((ol-mpv/uri/get-type "https://youtube.com/watch?v=FOOBAR") 'protocol))
		       )

;; Test for ol-mpv/uri/validate
(cort-deftest ol-mpv-test/uri/validate:protocol
		       '((:string= (ol-mpv/uri/validate "https://foo") "https://foo")))

(cort-deftest ol-mpv-test/uri/validate:filepath
		       '((:string= (ol-mpv/uri/validate "ol-mpv.el") (expand-file-name "ol-mpv.el"))))

(cort-deftest ol-mpv-test/uri/validate:unreachable
	      '((:cort-error 'ol-mpv/uri/unreachable (ol-mpv/uri/validate "NO_SUCH_FILE"))))

(cort-deftest ol-mpv-test/uri/validate:not-found
	      '((:cort-error 'ol-mpv/uri/not-found-error (ol-mpv/uri/validate nil))))
