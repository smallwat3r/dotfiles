;;; smallwat3r/dired/config.el -*- lexical-binding: t; -*-

;; Narrowing searches in dired
;; doc: https://github.com/Fuco1/dired-hacks
(use-package! dired-narrow
  :after dired
  :config
  (map! :map dired-mode-map
        :n "/" #'dired-narrow-fuzzy))

;; Toggle directories with TAB in dired
(use-package! dired-subtree
  :after dired
  :config
  (map! :map dired-mode-map
        "<tab>" #'dired-subtree-toggle
        "<backtab>" #'dired-subtree-cycle)

  ;; set a transparent background for all levels in dired subtree
  (custom-set-faces!
    `(,(cl-loop for i from 0 to 6 collect
                (intern (format "dired-subtree-depth-%d-face" i)))
      :background unspecified)))
