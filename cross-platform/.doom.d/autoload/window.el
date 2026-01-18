;;; $DOOMDIR/autoload/window.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/scroll-up ()
  "Scroll up by 3 lines."
  (interactive)
  (scroll-down 3))

;;;###autoload
(defun my/scroll-down ()
  "Scroll down by 3 lines."
  (interactive)
  (scroll-up 3))

;;;###autoload
(defun my/enlarge-window-horizontally ()
  "Enlarge window horizontally by 5 chars."
  (interactive)
  (enlarge-window-horizontally 5))

;;;###autoload
(defun my/shrink-window-horizontally ()
  "Shrink window horizontally by 5 chars."
  (interactive)
  (shrink-window-horizontally 5))

;;;###autoload
(defun my/enlarge-window ()
  "Enlarge window by 5 chars."
  (interactive)
  (enlarge-window 5))

;;;###autoload
(defun my/shrink-window ()
  "Shrink window by 5 chars."
  (interactive)
  (shrink-window 5))
