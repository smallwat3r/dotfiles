;;; smallwat3r/deft/config.el -*- lexical-binding: t; -*-

;; doc: https://github.com/jrblevin/deft/

(use-package! deft
  :commands (deft deft-new-file-named)
  :custom
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-auto-save-interval -1.0)  ; disable auto-save
  (deft-file-naming-rules
    '((noslash . "-")
      (nospace . "-")
      (case-fn . downcase)))
  :init (setq deft-default-extension "org")
  :config
  (set-evil-initial-state! 'deft-mode 'insert)  ; start on insert mode for filtering
  (add-hook 'deft-mode-hook #'doom-mark-buffer-as-real-h))
