;;; $DOOMDIR/autoload/process.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/fuzzy-kill-process ()
  "Fuzzy-pick a system process and send SIGKILL."
  (interactive)
  (let* ((fmt-item
          (lambda (pid)
            (let ((a (process-attributes pid)))
              (when a
                (let* ((name (alist-get 'comm a))
                       (pcpu (or (alist-get 'pcpu a) 0.0))
                       (rss  (alist-get 'rss a))
                       (rss-mb (if (numberp rss) (/ rss 1024.0) 0.0)))
                  (cons (format "%-20s %6d  %7.1fMB  %5.1f%%"
                                (or name "?") pid rss-mb pcpu)
                        pid))))))
         (items (delq nil (mapcar fmt-item (list-system-processes))))
         (choice (completing-read "Kill process: " (mapcar #'car items) nil t))
         (pid (cdr (assoc choice items))))
    (signal-process pid 'kill)
    (message "Killed: %s" choice)))
