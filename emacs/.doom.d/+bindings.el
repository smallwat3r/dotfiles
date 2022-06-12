;;; $DOOMDIR/+bindings.el -*- lexical-binding: t; -*-

;; This file bundles most of my custom bindings for using Doom Emacs. Some
;; bindings are not included here, when there are specific to packages and
;; cannot be bundled outside of the package configuration.

;; Disable bindings, due to other conflicts (specially coming from Hammerspoon).
(map! "M-k" nil
      "M-j" nil)

;; Bindings
(map!
 ;; For some reason, Emacs wouldn't print # on ^3, even when configured with
 ;; an English UK keyboard. This re-enabled this behaviour.
 (:map key-translation-map
  "M-3" "#")

 (:map evil-insert-state-map
  "C-h" #'evil-backward-char
  "C-l" #'evil-forward-char
  "C-k" #'evil-previous-line
  "C-j" #'evil-next-line)

 (:map evil-visual-state-map
  ";f"  #'+format/region)

 (:map evil-normal-state-map
  "C-2"   #'my/scroll-up
  "C-1"   #'my/scroll-down
  "S-C-h" #'my/enlarge-window-horizontally
  "S-C-l" #'my/shrink-window-horizontally
  "S-C-k" #'my/enlarge-window
  "S-C-j" #'my/shrink-window
  "M-SPC" #'cycle-spacing
  "M-o"   #'delete-blank-lines
  ";d"    #'my/save-and-close-buffer
  ";w"    #'my/save-buffer
  "C-k"   #'join-line
  "B"     #'beginning-of-line-text
  "E"     #'end-of-line)

 (:after python
  (:map python-mode-map
   (:leader
    (:localleader
     :desc "Open Python repl" "r" #'my/open-python-repl
     (:prefix ("e" . "env")
      :desc "Deactivate venv" "d" #'my/deactivate-python-venv
      :desc "Activate venv"   "a" #'my/activate-closest-python-venv)))))

 (:after dired
  (:map dired-mode-map
   "<tab>"     #'dired-subtree-toggle
   "<backtab>" #'dired-subtree-cycle
   :n "/"      #'dired-narrow-fuzzy))

 (:after vterm
  (:map vterm-mode-map
   :n "B"          #'vterm-beginning-of-line
   :n "<return>"   #'evil-insert-resume
   "<C-backspace>" #'my/vterm-delete-word
   :in "C-k"       #'vterm-send-up
   :in "C-j"       #'vterm-send-down))

 (:leader
  "§" #'other-frame
  "1" #'my/where-am-i

  (:prefix ("d" . "docker")
   :desc "List images"     "i" #'docker-images
   :desc "List containers" "c" #'docker-containers
   :desc "Exec into"       "e" #'docker-container-shell)

  (:prefix "b"
   :desc "Kill buffer" "d" #'my/kill-buffer)

  (:prefix "f"
   :desc "Find file in dotfiles" "P" #'my/find-file-in-dotfiles)

  (:prefix "o"
   :desc "Alacritty"              "a" #'my/alacritty-here
   :desc "Link at point"          "l" #'browse-url-at-point
   :desc "Vterm at root"          "T" #'+vterm/here
   :desc "Toggle vterm at root"   "t" #'+vterm/toggle
   :desc "Vterm at buffer"        "V" #'my/vterm/here-current-buffer
   :desc "Toggle vterm at buffer" "v" #'my/vterm/toggle-current-buffer
   :desc "Scr. buffer cur. mode"  "x" #'scratch
   :desc "Scr. buffer restclient" "h" #'my/scratch-rest-mode)

  (:prefix "t"
   :desc "Truncate lines" "t" #'toggle-truncate-lines)

  (:prefix "n"
   :desc "Open deft"     "d" #'deft
   :desc "Deft new file" "D" #'deft-new-file-named)

  (:prefix "c"
   :desc "Add overlay"     "h" #'symbol-overlay-put
   :desc "Remove overlays" "H" #'symbol-overlay-remove-all)

  (:prefix ("l" . "lorem")
   :desc "Insert paragraphs" "p" #'lorem-ipsum-insert-paragraphs
   :desc "Insert sentences"  "s" #'lorem-ipsum-insert-sentences
   :desc "Insert list"       "l" #'lorem-ipsum-insert-list)

  (:prefix "p"
   :desc "Run Makefile target" "m" #'+make/run)))
