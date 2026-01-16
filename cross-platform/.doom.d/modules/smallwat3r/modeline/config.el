;;; smallwat3r/modeline/config.el -*- lexical-binding: t; -*-

;; Show counter while in search modes
;; doc: https://github.com/emacsorphanage/anzu
(use-package! anzu
  :after-call isearch-mode)

;; doc: https://github.com/emacsorphanage/evil-anzu
(use-package! evil-anzu
  :after-call
  evil-ex-start-search
  evil-ex-start-word-search
  evil-ex-search-activate-highlight
  :config (global-anzu-mode +1))

(defvar my/buffer-count-cache 0
  "Cached count of user-visible buffers.")

(defun my/update-buffer-count ()
  "Update the cached buffer count."
  (setq my/buffer-count-cache
        (cl-count-if
         (lambda (b)
           (or (buffer-file-name b)
               (not (string-match "^ " (buffer-name b)))))
         (buffer-list))))

(add-hook 'buffer-list-update-hook #'my/update-buffer-count)

(defun my/number-of-buffers ()
  "Return the cached count of buffers."
  my/buffer-count-cache)

(let ((standard-mode-line-format
       (list "%e"
             'mode-line-front-space
             'mode-line-client
             'mode-line-modified
             'mode-line-remote
             'mode-line-frame-identification
             'mode-line-buffer-identification
             '(:eval (format "  b(%s)" (my/number-of-buffers)))
             " %p %l,%c  "
             '(vc-mode vc-mode)
             " "
             'mode-name
             " "
             'mode-line-misc-info
             'mode-line-end-spaces)))
  (setq-default mode-line-format standard-mode-line-format)
  (put 'mode-line-format 'standard-value
       (list `(quote ,standard-mode-line-format))))

(custom-set-faces!
  ;; make the active modeline stand out more
  '(mode-line :background "#ffb86c" :foreground "#1a1a1a" :weight bold))
