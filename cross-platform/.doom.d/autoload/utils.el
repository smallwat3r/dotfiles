;;; $DOOMDIR/autoload/utils.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/find-file-in-dotfiles ()
  "Find file in my dotfiles."
  (interactive)
  (doom-project-find-file my-dotfiles-dir))

;;;###autoload
(defun my/where-am-i ()
  "Display the current buffer's file path or buffer name."
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

;;;###autoload
(defun my/chatgpt-open-prompt ()
  "Open a popup buffer for a ChatGPT prompt."
  (interactive)
  (let* ((buf (get-buffer-create "*ChatGPT Prompt*"))
         (win (display-buffer
               buf
               '((display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25)))))
    (select-window win)
    (with-current-buffer buf
      (erase-buffer)
      (my-chatgpt-prompt-mode))))

;;;###autoload
(define-derived-mode my-chatgpt-prompt-mode text-mode "ChatGPT-Prompt"
  "Mode for composing ChatGPT prompts."
  (local-set-key (kbd "C-c C-c")
                 (lambda ()
                   (interactive)
                   (let* ((question (buffer-substring-no-properties
                                     (point-min) (point-max)))
                          (encoded (url-hexify-string question))
                          (url (concat "https://chatgpt.com/?prompt=" encoded)))
                     (browse-url url)
                     (quit-window t))))
  (local-set-key (kbd "C-c C-k") #'quit-window))
