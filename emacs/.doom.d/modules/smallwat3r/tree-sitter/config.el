;;; smallwat3r/tree-sitter/config.el -*- lexical-binding: t; -*-

;; doc: https://ubolonton.github.io/emacs-tree-sitter

(use-package! tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package! tree-sitter-langs
  :after tree-sitter
  :config
  (defvar my-tree-sitter-ignore-faces
    '("property" "operator" "method.call" "function.call" "label")
    "Alist of tree-sitter face attributes to ignore.")

  ;; Deactivate faces on some specific programming nodes, as I find this
  ;; makes the buffer too busy and difficult to read.
  (add-function :before-while tree-sitter-hl-face-mapping-function
                (lambda (capture-name)
                  (not (member capture-name my-tree-sitter-ignore-faces))))

  (tree-sitter-hl-add-patterns 'python
    [((string) @doc
      (.match? @doc "^(\"\"\"|r\"\"\")"))]))
