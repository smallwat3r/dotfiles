;;; smallwat3r/evil-ext/config.el -*- lexical-binding: t; -*-

;; Evil-mode
(after! evil
  (setq evil-vsplit-window-right t
        evil-split-window-below t
        evil-want-fine-undo t)

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
  (setq evil-snipe-scope 'visible))
