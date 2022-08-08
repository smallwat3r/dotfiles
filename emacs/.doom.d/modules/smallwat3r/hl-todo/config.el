;;; smallwat3r/hl-todo/config.el -*- lexical-binding: t; -*-

;; Highlight todos comments
;; doc: https://github.com/tarsius/hl-todo
(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (defface my-todos-face
    '((t :background "grey96"
         :foreground "dark red"
         :weight bold
         :inherit help-key-binding))
    "The face used to display todos from hl-todo.")

  (setq hl-todo-keyword-faces
        `(("TODO" . my-todos-face)
          ("DEPRECATED" . my-todos-face)
          ("FIXME" . my-todos-face)
          ("HACK" . my-todos-face)
          ("BUG" . my-todos-face)
          ("NOTE" . my-todos-face))))
