;;; $DOOMDIR/+custom-faces.el -*- lexical-binding: t; -*-

;; Lets use a default theme as a base and override some faces to my liking

(setq doom-theme 'modus-vivendi)

(setq modus-themes-slanted-constructs nil
      modus-themes-bold-constructs nil
      modus-themes-syntax '(faint yellow-comments)
      modus-themes-mode-line '3d
      modus-themes-completions 'opinionated
      modus-themes-org-blocks 'tinted-background)

(setq modus-themes-vivendi-color-overrides
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

(custom-set-faces!
  '((term
     magit-diff-context-highlight
     fringe
     flycheck-error
     flycheck-warning)
    :background unspecified)

  '(highlight-numbers-number :foreground "#8e7cc3")

  '(diff-refine-added :inherit magit-diff-added-highlight :inverse-video nil :weight bold)
  '(diff-refine-removed :inherit magit-diff-removed-highlight :inverse-video nil :weight bold)
  '(diff-refine-changed :inverse-video nil :weight bold)
  '(git-gutter-fr:added :foreground "lime green")
  '(git-gutter-fr:modified :foreground "turquoise")
  '(git-gutter-fr:deleted :foreground "red")

  '(flycheck-error :underline (:color "red" :style wave))
  '(flycheck-warning :underline (:color "orange" :style wave))

  '(slack-preview-face :background unspecified)
  '((slack-mrkdwn-code-face slack-mrkdwn-code-block-face)
    :foreground "grey51" :inherit fixed-pitch)

  ;; Remove some code syntax highlighting, keep it simple, but we then need to re-map some
  ;; of the colors from web-mode, just so we can have some syntax highlighting when reading
  ;; HTML code
  '((font-lock-function-name-face
     font-lock-variable-name-face
     font-lock-constant-face
     font-lock-type-face)
    :foreground unspecified :weight normal)
  '((web-mode-html-attr-equal-face
     web-mode-html-attr-name-face
     web-mode-html-tag-face)
    :inherit font-lock-keyword-face)
  '(web-mode-html-tag-bracket-face :inherit font-lock-builtin-face))
