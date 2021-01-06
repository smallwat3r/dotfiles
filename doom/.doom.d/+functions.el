;;; $DOOMDIR/+functions.el -*- lexical-binding: t; -*-

(defun zz/add-scratch-buffer-header (text)
  "Add an automatic header to a scratch buffer."
  (when scratch-buffer
    (save-excursion
      (goto-char (point-min))
      (insert text)
      (newline 2))
    (end-of-buffer)))

(defun zz/echo-command-name ()
  "Echo the command names in minibuffer as they are being used.
Some obvious commands are hidden to remove any fluff."
  (unless
      (or (eq this-command 'self-insert-command)
          (eq this-command 'evil-backward-char)
          (eq this-command 'evil-forward-char)
          (eq this-command 'scroll-up-line)
          (eq this-command 'scroll-down-line)
          (eq this-command 'previous-line)
          (eq this-command 'next-line))
    (message "%s" this-command)))

;; https://emacs.stackexchange.com/a/5583
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

;; https://news.ycombinator.com/item?id=22131815
(defun zz/arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))
