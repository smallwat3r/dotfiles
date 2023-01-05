;;; smallwat3r/tree-sitter/config.el -*- lexical-binding: t; -*-

;; Tree-sitter is a parser generator tool and an incremental parsing library.
;; It can build a concrete syntax tree for a source file and efficiently update
;; the syntax tree as the source file is edited.
;; This also provides faster and better syntax highlighting.
;; doc: https://ubolonton.github.io/emacs-tree-sitter
(use-package! tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package! tree-sitter-langs
  :after tree-sitter
  :config
  ;; Deactivate faces on some specific programming nodes, as I find this makes
  ;; the buffer too busy and difficult to read.
  (add-function :before-while tree-sitter-hl-face-mapping-function
                (lambda (capture-name)
                  (not (member capture-name
                               '("property" "operator" "method.call" "function.call"
                                 "function.method.call" "function.special" "label")))))

  ;; Fix rendering python docstring apostrophes.
  (tree-sitter-hl-add-patterns 'python
    [((string) @doc
      (.match? @doc "^(\"\"\"|r\"\"\")"))]))
