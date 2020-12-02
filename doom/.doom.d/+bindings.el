;;; $DOOMDIR/+bindings.el -*- lexical-binding: t; -*-

(map!
 ;; Scrolling
 "C-j"       #'scroll-up-line
 "C-k"       #'scroll-down-line

 (:map override
  ;; Resize split windows
  "S-C-h"    #'shrink-window-horizontally
  "S-C-l"    #'enlarge-window-horizontally
  "S-C-k"    #'enlarge-window
  "S-C-j"    #'shrink-window

  ;; Change windows
  "M-h"      #'windmove-left
  "M-l"      #'windmove-right
  "M-k"      #'windmove-up
  "M-j"      #'windmove-down

  "M-n"      #'+default/new-buffer
  "M-`"      #'other-frame

  ;; MacOS UK keyboard hash key hack
  "M-3"      "#")

 ;; Vim-like stuff
 (:map evil-normal-state-map
  ";f"       #'format-all-buffer
  ";w"       #'evil-write
  ";q"       #'evil-save-and-close
  ";x"       #'evil-save-and-close
  ";vs"      #'split-window-horizontally
  ";vw"      #'evil-window-vnew
  ";sp"      #'split-window-vertically
  ";sw"      #'evil-window-new)

 (:leader

  ;; Buffers
  (:prefix "b"
   :desc "Kill buffer"              "d" #'evil-delete-buffer)

  ;; Open
  (:prefix "o"
   :desc "Reveal in Finder"         "o" #'+macos/reveal-in-finder
   :desc "Reveal project in Finder" "O" #'+macos/reveal-project-in-finder)

  ;; Errors
  (:prefix ("e" . "errors")
   :desc "Flycheck list errors"     "l" #'flycheck-list-errors
   :desc "Flycheck next error"      "n" #'flycheck-next-error
   :desc "Flycheck previous error"  "p" #'flycheck-previous-error
   :desc "Flycheck explain error"   "e" #'flycheck-explain-error-at-point
   :desc "Flycheck verify setup"    "v" #'flycheck-verify-setup)

  ;; Kubernetes
  (:prefix ("k" . "kubernetes")
   :desc "Overview"                 "o" #'kubernetes-overview
   :desc "Set namespace"            "n" #'kubernetes-set-namespace
   :desc "Describe"                 "d" #'kubernetes-describe-pod
   :desc "Exec into"                "e" #'kubernetes-exec-into)

  ;; Search
  (:prefix "s"
   :desc "Ripgrep"                  "g" #'deadgrep))

 ;; Org journal
 (:after org-journal
  :leader
  (:prefix ("j" . "journal")
   :desc "Search journal"           "s" #'org-journal-search
   :desc "New journal entry"        "n" #'org-journal-new-entry))

 ;; Python stuff
 (:after python
  :leader
  :map python-mode-map

  ;; Imports
  (:prefix ("I" . "imports")
   :desc "Isort buffer"             "s" #'+python/optimize-imports)

  ;; Venv
  (:prefix ("v" . "venv")
   :desc "Workon"                   "w" #'pyvenv-workon
   :desc "Activate pyvenv"          "a" #'pyvenv-activate
   :desc "Deactivate pyvenv"        "d" #'pyvenv-deactivate))
 )