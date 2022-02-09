;;; smallwat3r/deft/config.el -*- lexical-binding: t; -*-

;; doc: https://github.com/jrblevin/deft/

(use-package! deft
  :commands (deft deft-new-file-named)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-auto-save-interval 0)  ; disable auto-save
  (deft-extensions '("org" "md" "txt"))
  (deft-file-naming-rules
    '((noslash . "-")
      (nospace . "-")
      (case-fn . downcase)))
  :config
  (set-evil-initial-state! 'deft-mode 'insert)  ; start on insert mode for filtering
  (add-hook 'deft-mode-hook #'doom-mark-buffer-as-real-h))
