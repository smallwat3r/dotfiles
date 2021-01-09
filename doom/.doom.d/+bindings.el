;;; $DOOMDIR/+bindings.el -*- lexical-binding: t; -*-

(map!

 ;; Hack for hash key support, on UK macOS keyboard, M-3 wouldn't print a hash (#)
 (:map key-translation-map "M-3" "#")

 ;; Normal mode bindings
 (:map evil-normal-state-map
  ;; Scrollin
  "C-j"      #'scroll-up-line
  "C-k"      #'scroll-down-line

  ;; Shrink and enlarge windows
  "S-C-h"    #'shrink-window-horizontally
  "S-C-l"    #'enlarge-window-horizontally
  "S-C-k"    #'enlarge-window
  "S-C-j"    #'shrink-window

  ;; Toggle spacing options
  "M-SPC"    #'cycle-spacing

  ;; Delete blank lines below cursor position
  "M-o"      #'delete-blank-lines

  ;; Cycle through frames
  "M-`"      #'other-frame

  ;; NOTE: Using semicolon (;) as a sort of leader to perform
  ;; general actions in normal mode (as I used to do in vim)

  ;; Auto-format
  ";f"       #'format-all-buffer

  ;; General actions (write, save, close etc)
  ";w"       #'evil-write
  ";x"       #'evil-save
  ";q"       #'evil-save-and-close

  ;; Splitting current buffer
  ";vs"      #'evil-window-vsplit ; vertical
  ";sp"      #'evil-window-split  ; horizontal

  ;; Create new window (split screen)
  ";vw"      #'evil-window-vnew   ; vertical
  ";sw"      #'evil-window-new    ; horizontal

  ;; Clear search highlights
  ";,"       #'evil-ex-nohighlight)

 ;; Insert and Normal mode bindings
 (:map (evil-insert-state-map evil-normal-state-map)
  ;; Join lines instead of deleting region
  "M-k"      #'evil-join)

 ;; localleader
 (:leader
  (:prefix "m"
   :desc "Makefile run"             "r" #'+make/run))

 ;; Buffers
 (:leader
  (:prefix "b"
   :desc "Kill buffer"              "d" #'evil-delete-buffer
   :desc "Pop up scratch buffer"    "x" #'scratch))

 ;; Pop up scratch buffer with current mode
 (:leader
  :desc "Pop up scratch buffer"     "x" #'scratch)

 ;; Open
 (:leader
  (:prefix "o"
   :desc "Emails"                   "m" #'notmuch
   :desc "Reveal in Finder"         "o" #'+macos/reveal-in-finder
   :desc "Reveal project in Finder" "O" #'+macos/reveal-project-in-finder))

 ;; Toggles
 (:leader
  (:prefix "t"
   :desc "Rainbow mode"             "c" #'rainbow-mode))

 ;; Errors
 (:leader
  (:prefix ("e" . "errors")
   :desc "Flycheck list errors"     "l" #'flycheck-list-errors
   :desc "Flycheck next error"      "n" #'flycheck-next-error
   :desc "Flycheck previous error"  "p" #'flycheck-previous-error
   :desc "Flycheck explain error"   "e" #'flycheck-explain-error-at-point
   :desc "Flycheck verify setup"    "v" #'flycheck-verify-setup))

 ;; Kubernetes
 (:leader
  (:prefix ("k" . "kubernetes")
   :desc "Overview"                 "o" #'kubernetes-overview
   :desc "Set context"              "c" #'kubernetes-use-context
   :desc "Set namespace"            "n" #'kubernetes-set-namespace
   :desc "Display logs"             "l" #'kubernetes-logs-fetch-all
   :desc "Display service"          "s" #'kubernetes-display-service
   :desc "Display deployment"       "d" #'kubernetes-display-deployment
   :desc "Describe"                 "D" #'kubernetes-describe-pod
   :desc "Exec into"                "e" #'kubernetes-exec-into))

 ;; Docker
 (:leader
  (:prefix ("d" . "docker")
   :desc "List images"              "i" #'docker-images
   :desc "List containers"          "c" #'docker-containers
   :desc "Exec into"                "e" #'docker-container-shell))

 ;; Search
 (:leader
  (:prefix "s"
   :desc "Ripgrep"                  "g" #'ripgrep-regexp))

 ;; Notes
 (:leader
  (:prefix "n"
   :desc "Deft open"                "D" #'deft
   :desc "Deft new"                 "d" #'deft-new-file))

 ;; Org-journal
 (:after org-journal
  :leader
  (:prefix ("j" . "journal")
   :desc "Search journal"           "s" #'org-journal-search
   :desc "New journal entry"        "n" #'org-journal-new-entry))

 ;; Python
 (:map python-mode-map
  :after python
  :localleader
  (:prefix ("v" . "venv")  ; virtual env
   :desc "Workon"                   "w" #'pyvenv-workon
   :desc "Activate pyvenv"          "a" #'pyvenv-activate
   :desc "Deactivate pyvenv"        "d" #'pyvenv-deactivate))

 )
