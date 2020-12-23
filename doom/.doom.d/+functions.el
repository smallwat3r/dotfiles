;;; $DOOMDIR/+functions.el -*- lexical-binding: t; -*-

;; Insert hex color (https://emacs.stackexchange.com/a/5583)
(defun zz/insert-color-hex (&optional arg)
  "Select a color and insert its 24-bit hexadecimal RGB format.

With prefix argument \\[universal-argument] insert the 48-bit value."
  (interactive "*P")
  (let ((buf (current-buffer)))
    (list-colors-display
     nil nil `(lambda (name)
                (interactive)
                (quit-window)
                (with-current-buffer ,buf
                  (insert (apply #'color-rgb-to-hex
                                 (nconc (color-name-to-rgb name)
                                        (unless (consp ',arg)
                                          (list (or ,arg 2)))))))))))

;; source: https://news.ycombinator.com/item?id=22131815
(defun zz/arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))
