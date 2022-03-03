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
  (deft-use-filename-as-title t))
