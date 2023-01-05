;;; smallwat3r/scratch/config.el -*- lexical-binding: t; -*-

;; Scratch buffers
;; doc: https://github.com/ieure/scratch-el
(use-package! scratch
  :commands (scratch)
  :hook
  (org-mode . my/scratch-buffer-org-mode)
  (sh-mode . my/scratch-buffer-sh-mode)
  (restclient-mode . my/scratch-buffer-restclient-mode)
  :config
  (defun my/scratch--add-buffer-header (text)
    "Open scratch buffer with a TEXT header."
    (when scratch-buffer
      (save-excursion
        (goto-char (point-min))
        (insert text)
        (newline 2))
      (goto-char (point-max))))

  (defun my/scratch-buffer-org-mode ()
    (my/scratch--add-buffer-header "#+TITLE: Scratch file"))

  (defun my/scratch-buffer-sh-mode ()
    (my/scratch--add-buffer-header "#!/usr/bin/env bash"))

  (defun my/scratch-buffer-restclient-mode ()
    (my/scratch--add-buffer-header "#\n# restclient\n#")))
