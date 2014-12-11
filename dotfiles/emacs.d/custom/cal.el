(setq calendar-latitude 37.813
      calendar-longitude -122.256
      calendar-location-name "Oakland, CA")

(setq diary-file (file-truename "~/.emacs.d/diary"))
(setq calendar-mark-holidays-flag t) ;; Show holidays
(custom-set-faces
 '(calendar-weekend-header
   ((t(:foreground "color-198"
		   :weight extra-bold))))
 '(holiday ((t (:background "color-202")))))
