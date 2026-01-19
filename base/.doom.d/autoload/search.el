;;; $DOOMDIR/autoload/search.el -*- lexical-binding: t; -*-
;;
;; File and project search helpers.

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
