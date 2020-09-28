;;; $DOOMDIR/+bindings.el -*- lexical-binding: t; -*-

(map!
 ;; scrolling
 "C-j" #'scroll-up-line
 "C-k" #'scroll-down-line

 (:map override
  ;; resize split windows
  "S-C-h" #'shrink-window-horizontally
  "S-C-l" #'enlarge-window-horizontally
  "S-C-k" #'enlarge-window
  "S-C-j" #'shrink-window

  ;; move windows
  "M-h" #'windmove-left
  "M-l" #'windmove-right
  "M-k" #'windmove-up
  "M-j" #'windmove-down

  ;; macOS UK keyboard hash key hack
  "M-3" "#")

 ;; vim-like stuff
 (:map evil-normal-state-map
  ";f"  #'format-all-buffer
  ";w"  #'evil-write
  ";q"  #'evil-save-and-close
  ";x"  #'evil-save-and-close
  ";vs" #'split-window-horizontally
  ";sp" #'split-window-vertically)

 ;; leader bindings
 (:leader
  :desc "Next Error"            :n  "]"  #'flycheck-next-error
  :desc "Previous Error"        :n  "["  #'flycheck-previous-error
  :desc "Show flycheck errors"  :n  "!"  #'flycheck-list-errors

  (:desc "open" :prefix "o"
   :desc "Kubernetes"           :n  "K" #'kubernetes-overview))
 )
