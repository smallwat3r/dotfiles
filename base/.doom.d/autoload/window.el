;;; $DOOMDIR/autoload/window.el -*- lexical-binding: t; -*-
;;
;; Window scrolling and resizing helpers.

;;;###autoload
(defun my/scroll-up ()
  "Scroll view up by 3 lines."
  (interactive)
  (scroll-down 3))

;;;###autoload
(defun my/scroll-down ()
  "Scroll view down by 3 lines."
  (interactive)
  (scroll-up 3))

;;;###autoload
(defun my/enlarge-window-horizontally ()
  "Enlarge window horizontally by 5 columns."
  (interactive)
  (enlarge-window-horizontally 5))

;;;###autoload
(defun my/shrink-window-horizontally ()
  "Shrink window horizontally by 5 columns."
  (interactive)
  (shrink-window-horizontally 5))

;;;###autoload
(defun my/enlarge-window ()
  "Enlarge window vertically by 5 lines."
  (interactive)
  (enlarge-window 5))

;;;###autoload
(defun my/shrink-window ()
  "Shrink window vertically by 5 lines."
  (interactive)
  (shrink-window 5))
