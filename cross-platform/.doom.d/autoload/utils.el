;;; $DOOMDIR/autoload/utils.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/find-file-in-dotfiles ()
  "Find file in my dotfiles."
  (interactive)
  (doom-project-find-file my-dotfiles-dir))

;;;###autoload
(defun my/where-am-i ()
  "Echo where I'm at."
  (interactive)
  (message (if (buffer-file-name)
               (buffer-file-name)
             (concat "buffer-name=" (buffer-name)))))

;;;###autoload
(defun my/vertico-search-project-symbol-at-point (&optional arg)
  "Performs a live project search from the project root for the thing at point."
  (interactive)
  (+vertico/project-search arg (thing-at-point 'symbol)))

;;;###autoload
(defun my/open-remote-conn ()
  "Open remote SSH connection with Tramp."
  (interactive)
  (let ((tramp-path "/ssh:")
        (prompt "Pick target: "))
    (find-file (read-file-name prompt tramp-path))))

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
