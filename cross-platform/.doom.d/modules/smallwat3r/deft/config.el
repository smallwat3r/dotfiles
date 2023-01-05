;;; smallwat3r/deft/config.el -*- lexical-binding: t; -*-

;; Deft allows quick taking and editing of notes within Emacs.
;; Doom's configuration provides a default deft module, but I prefer managing
;; my own as the configurations are very light.
;; doc: https://github.com/jrblevin/deft/
(use-package! deft
  :commands deft
  :init (map! (:leader (:prefix "n" :desc "Open deft" "d" #'deft)))
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-auto-save-interval 0)  ; disable auto-save
  (deft-extensions '("org" "md" "txt"))
  (deft-file-naming-rules
    '((noslash . "-")
      (nospace . "-")
      (case-fn . downcase)))
  (deft-use-filename-as-title t)
  :config
  (map!
   (:map deft-mode-map
         (:leader
          (:localleader
           "RET" #'deft-new-file-named
           "c"   #'deft-filter-clear
           "d"   #'deft-delete-file
           "f"   #'deft-find-file
           "g"   #'deft-refresh
           "n"   #'deft-new-file
           "r"   #'deft-rename-file)))))
