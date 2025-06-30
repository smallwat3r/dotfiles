;;; smallwat3r/scratch/config.el -*- lexical-binding: t; -*-

;; Scratch buffers
;; doc: https://github.com/ieure/scratch-el
(use-package! scratch
  :commands scratch
  :init
  (map! :leader
        :prefix-map ("o" . "open")
        (:prefix ("s" . "Scratch buffer")
         :desc "Current mode" "o" #'scratch
         :desc "Restclient"   "r" #'my/scratch-rest-mode))
  :config
  (defun my/scratch-add-buffer-header (text)
    "Open scratch buffer with a TEXT header."
    (when scratch-buffer
      (save-excursion
        (goto-char (point-min))
        (insert text)
        (newline 2))
      (goto-char (point-max))))

  (defun my/scratch-buffer-org-mode ()
    (my/scratch-add-buffer-header "#+TITLE: Scratch file"))

  (add-hook! 'org-mode-hook #'my/scratch-buffer-org-mode)

  (defun my/scratch-buffer-sh-mode ()
    (my/scratch-add-buffer-header "#!/usr/bin/env bash"))

  (add-hook! 'sh-mode-hook #'my/scratch-buffer-sh-mode)

  (defun my/scratch-buffer-restclient-mode ()
    (my/scratch-add-buffer-header "#\n# restclient\n#"))

  (add-hook! 'restclient-mode-hook #'my/scratch-buffer-restclient-mode))
