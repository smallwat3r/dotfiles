;;; smallwat3r/csv/config.el -*- lexical-binding: t; -*-

;; CSV mode
;; doc: https://elpa.gnu.org/packages/csv-mode.html
(use-package! csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :config
  (map! :map csv-mode-map
        :leader
        :localleader
        :desc "CSV align fields" "a" #'csv-align-fields
        :desc "CSV unalign fields" "A" #'csv-unalign-fields
        :desc "CSV toggle sticky header" "h" #'csv-header-line))
