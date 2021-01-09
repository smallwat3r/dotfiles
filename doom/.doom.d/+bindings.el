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

 ;; Markdown
 (:leader
  (:map markdown-mode-map
   (:prefix ("I" . "md-insert")
    :desc "Blockquote"    "q" #'markdown-insert-blockquote
    :desc "Bold"          "b" #'markdown-insert-bold
    :desc "Code"          "c" #'markdown-insert-code
    :desc "Emphasis"      "e" #'markdown-insert-italic
    :desc "Footnote"      "f" #'markdown-insert-footnote
    :desc "Code Block"    "s" #'markdown-insert-gfm-code-block
    :desc "Image"         "i" #'markdown-insert-image
    :desc "Link"          "l" #'markdown-insert-link
    :desc "List Item"     "n" #'markdown-insert-list-item
    :desc "Pre"           "p" #'markdown-insert-pre)
   (:prefix ("H" . "md-headings")
    :desc "One"           "1" #'markdown-insert-header-atx-1
    :desc "Two"           "2" #'markdown-insert-header-atx-2
    :desc "Three"         "3" #'markdown-insert-header-atx-3
    :desc "Four"          "4" #'markdown-insert-header-atx-4
    :desc "Five"          "5" #'markdown-insert-header-atx-5
    :desc "Six"           "6" #'markdown-insert-header-atx-6)))

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
   :desc "Set namespace"            "n" #'kubernetes-set-namespace
   :desc "Display service"          "s" #'kubernetes-display-service
   :desc "Display deployment"       "d" #'kubernetes-display-deployment
   :desc "Describe"                 "D" #'kubernetes-describe-pod
   :desc "Exec into"                "e" #'kubernetes-exec-into))

 ;; Docker
 (:leader
  (:prefix ("d" . "docker")
   :desc "Images"                   "i" #'docker-images
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
 (:after python
  :leader
  :map python-mode-map
  (:prefix ("I" . "imports")  ; imports
   :desc "Isort buffer"             "s" #'+python/optimize-imports)
  (:prefix ("v" . "venv")  ; virtual env
   :desc "Workon"                   "w" #'pyvenv-workon
   :desc "Activate pyvenv"          "a" #'pyvenv-activate
   :desc "Deactivate pyvenv"        "d" #'pyvenv-deactivate))

 )
