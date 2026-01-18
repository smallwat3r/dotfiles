;;; smallwat3r/org-ext/config.el -*- lexical-binding: t; -*-

;; Org mode
;; doc: https://orgmode.org/manual/
(after! org
  (setq org-directory my-notes-directory
        org-hide-emphasis-markers nil
        org-pretty-entities t
        org-ellipsis "...")

  ;; Do not wrap lines when converting to markdown.
  (setq org-pandoc-options-for-markdown '((wrap . "none"))))

;; Org modern
;; doc: https://github.com/minad/org-modern
(use-package! org-modern
  :hook (org-mode . org-modern-mode))

;; Journal
;; doc: https://github.com/bastibe/org-journal
(after! org-journal
  (setq org-journal-dir (expand-file-name "journal/" my-notes-directory)
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-format "journal-%Y%m%d.org"))
