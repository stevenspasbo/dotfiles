
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(setq calendar-latitude 37.813
      calendar-longitude -122.256
      calendar-location-name "Oakland, CA" ; Show my location to the internet
      diary-file (concat (getenv "HOME") "/Dropbox/diary.org") ; Sets diary file
      calendar-mark-holidays-flag nil) ; Show holidays
