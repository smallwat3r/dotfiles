;;; smallwat3r/modeline/config.el -*- lexical-binding: t; -*-

;; Show counter while in search modes
;; doc: https://github.com/emacsorphanage/anzu
(use-package! anzu
  :after-call isearch-mode)

;; doc: https://github.com/emacsorphanage/evil-anzu
(use-package! evil-anzu
  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :config (global-anzu-mode +1))

(defun my/number-of-buffers ()
  "Count the number of buffers."
  (cl-count-if
   (lambda (b)
     (or (buffer-file-name b)
         (not (string-match "^ " (buffer-name b)))))
   (buffer-list)))

(let ((standard-mode-line-format
       (list "%e"
             'mode-line-front-space
             'mode-line-client
             'mode-line-modified
             'mode-line-remote
             "%12b"
             '(vc-mode vc-mode)
             '(:eval (format "  b(%s)" (my/number-of-buffers)))
             " %p %l,%c  "
             'mode-name
             " "
             'mode-line-misc-info
             'mode-line-end-spaces)))
  (setq-default mode-line-format standard-mode-line-format))
