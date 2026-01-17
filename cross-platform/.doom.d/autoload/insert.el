;;; $DOOMDIR/autoload/insert.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/insert-timestamp (&optional datetime)
  "Insert current date or date+time."
  (interactive "P")
  (let ((fmt (if datetime "%Y-%m-%d %H:%M" "%Y-%m-%d")))
    (insert (format-time-string fmt))))

;;;###autoload
(defun my/insert-email ()
  "Insert an email address from `my-email-addresses-alist'."
  (interactive)
  (let* ((keys (mapcar #'car my-email-addresses-alist))
         (choice (completing-read "Email: " keys nil t)))
    (insert (my/get-email choice))))
