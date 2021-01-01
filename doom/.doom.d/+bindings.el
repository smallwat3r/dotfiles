;;; $DOOMDIR/+bindings.el -*- lexical-binding: t; -*-

(map!

 ;; Defaults
 "C-j"       #'scroll-up-line  ; scroll
 "C-k"       #'scroll-down-line

 ;; Override
 (:map override
  "S-C-h"    #'shrink-window-horizontally  ; window sizes
  "S-C-l"    #'enlarge-window-horizontally
  "S-C-k"    #'enlarge-window
  "S-C-j"    #'shrink-window

  "M-h"      #'windmove-left  ; switch windows
  "M-l"      #'windmove-right
  "M-k"      #'windmove-up
  "M-j"      #'windmove-down

  "M-SPC"    #'cycle-spacing
  "M-o"      #'delete-blank-lines

  "M-n"      #'+default/new-buffer
  "M-`"      #'other-frame)

 ;; Normal mode
 (:map evil-normal-state-map
  ";f"       #'format-all-buffer
  ";w"       #'evil-write
  ";x"       #'evil-save
  ";q"       #'evil-save-and-close
  ";vs"      #'evil-window-vsplit
  ";vw"      #'evil-window-vnew
  ";sp"      #'evil-window-split
  ";sw"      #'evil-window-new
  ";,"       #'evil-ex-nohighlight)

 ;; Pop up buffer with current mode
 (:leader
  :desc "Pop up scratch buffer"  "x" #'scratch)

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
   :desc "Kill buffer"              "d" #'evil-delete-buffer))

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
