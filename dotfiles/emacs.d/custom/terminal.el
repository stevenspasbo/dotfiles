;;;; Window stuff, line nums, menu, etc

(global-linum-mode 1)  ; Enable line numbers
(column-number-mode 1) ; Enable (line,column)
(menu-bar-mode -1)     ; Disable menu

(when window-system    ; If standalone emacs application
  (scroll-bar-mode -1) ; Disable scroll bar
  (tool-bar-mode -1)   ; Disable tool bar
  )
