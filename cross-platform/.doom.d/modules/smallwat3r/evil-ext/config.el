;;; smallwat3r/evil-ext/config.el -*- lexical-binding: t; -*-

;; Evil-mode
(after! evil
  ;; General evil mode settings.
  ;; Note: "custom layout" comments indicate alternative bindings for the
  ;; Smallcat keyboard (26 keys), where y/n/a/e/i replace the standard
  ;; h/j/k/l vim keys.
  (setq evil-vsplit-window-right t
        evil-split-window-below t
        evil-want-fine-undo t)

  (map! :map evil-insert-state-map
        "C-h" #'evil-backward-char
        "C-l" #'evil-forward-char
        "C-k" #'evil-previous-line
        "C-j" #'evil-next-line

        :map evil-visual-state-map
        ";f"  #'+format/region

        :map evil-normal-state-map
        ;; Window scrolling
        "C-;"   #'my/scroll-up
        "C-l"   #'my/scroll-down
        "["     #'my/scroll-up
        "]"     #'my/scroll-down

        ;; Window resizing
        "S-C-h" #'my/enlarge-window-horizontally
        "S-C-y" #'my/enlarge-window-horizontally  ; custom layout
        "S-C-l" #'my/shrink-window-horizontally
        "S-C-i" #'my/shrink-window-horizontally  ; custom layout
        "S-C-k" #'my/enlarge-window
        "S-C-a" #'my/enlarge-window  ; custom layout
        "S-C-j" #'my/shrink-window
        "S-C-n" #'my/shrink-window  ; custom layout

        ;; Misc editing
        "M-SPC" #'cycle-spacing
        "M-o"   #'delete-blank-lines
        "C-k"   #'join-line
        "C-a"   #'join-line  ; custom layout
        "B"     #'beginning-of-line-text
        "E"     #'end-of-line
        "M-<delete>" #'kill-word
        "C-n"   #'electric-newline-and-maybe-indent

        ;; Buffer management
        ";d"    #'my/save-and-close-buffer
        ";w"    #'save-buffer
        ";s"    #'save-buffer
        ";q"    #'my/kill-buffer

        :leader
        ;; Window management
        "wy" #'evil-window-left
        "ly" #'evil-window-left  ; custom layout
        "wn" #'evil-window-down
        "ln" #'evil-window-down  ; custom layout
        "wa" #'evil-window-up
        "la" #'evil-window-up    ; custom layout
        "we" #'evil-window-right
        "le" #'evil-window-right ; custom layout
        "ls" #'evil-window-split
        "lv" #'evil-window-vsplit)

  ;; Change the cursor color depending on the evil mode
  (setq evil-default-state-cursor  '(box "cyan3")
        evil-normal-state-cursor   '(box "cyan3")
        evil-insert-state-cursor   '(bar "green3")
        evil-visual-state-cursor   '(box "OrangeRed2")
        evil-replace-state-cursor  '(hbar "red2")
        evil-operator-state-cursor '(box "red2")))

;; Evil visual hints when yanking, pasting, deleting etc.
;; doc: https://github.com/edkolev/evil-goggles
(after! evil-goggles
  (setq evil-goggles-duration 0.15)
  (evil-goggles-use-diff-refine-faces))

;; Evil snipe
;; doc: https://github.com/hlissner/evil-snipe
(after! evil-snipe
  (setq evil-snipe-scope 'visible)
  (map! :map evil-snipe-parent-transient-map
        :g "j" #'evil-snipe-repeat
        :g "k" #'evil-snipe-repeat-reverse
        :g "n" #'evil-snipe-repeat         ; custom layout
        :g "a" #'evil-snipe-repeat-reverse))
