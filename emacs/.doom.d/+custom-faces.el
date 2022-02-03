;;; $DOOMDIR/+custom-faces.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-solarized-dark-high-contrast)

;; When using modus themes, let's change some default colors and settings
(setq modus-themes-slanted-constructs nil
      modus-themes-bold-constructs nil
      modus-themes-syntax '(faint yellow-comments)
      modus-themes-mode-line '3d
      modus-themes-completions 'opinionated
      modus-themes-org-blocks 'tinted-background
      modus-themes-vivendi-color-overrides
      '((bg-main . "grey0")
        (fg-main . "grey88")
        (magenta . "#b5b600")
        (magenta-alt . "#b5b600")
        (magenta-alt-faint . "#b5b600")
        (magenta-faint . "#b5b600")
        (magenta-alt-other . "#57c8ca")
        (magenta-alt-other-faint . "#57c8ca")
        (blue-faint . "#c27ba0")
        (blue-alt-faint . "#c27ba0")))

;; Lets override some faces to my liking
(custom-set-faces!
  '(default :background "black")  ; enforce a black bg
  '(cursor :background "#d7ff00")

  '((term
     magit-diff-context-highlight
     fringe
     flycheck-error
     flycheck-warning
     markdown-inline-code-face
     markdown-language-keyword-face
     diff-context
     magit-diff-context
     slack-preview-face)
    :background unspecified)

  '(diff-refine-added :inherit magit-diff-added-highlight
                      :inverse-video nil
                      :weight bold
                      :foreground "green")
  '(diff-refine-removed :inherit magit-diff-removed-highlight
                        :inverse-video nil
                        :weight bold
                        :foreground "red")
  '(diff-refine-changed :inverse-video nil
                        :weight bold
                        :foreground "blue")

  '(font-lock-warning-face :foreground "red"
                           :underline (:color "red" :style wave))

  '(flycheck-error :underline (:color "red" :style wave))
  '(flycheck-warning :underline (:color "orange" :style wave))

  '((slack-mrkdwn-code-face slack-mrkdwn-code-block-face)
    :foreground "grey51" :inherit fixed-pitch)

  ;; Keep it really simple and ignore most of the code syntax highlighting, so its
  ;; easy on the eyes and its better to focus. This will still keep settings for any
  ;; italics faces.
  '((font-lock-function-name-face
     font-lock-variable-name-face
     font-lock-constant-face
     font-lock-type-face
     font-lock-builtin-face)
    :foreground unspecified :weight normal)

  '((web-mode-html-attr-equal-face
     web-mode-html-attr-name-face
     web-mode-html-tag-face
     makefile-targets)
    :inherit font-lock-keyword-face))
