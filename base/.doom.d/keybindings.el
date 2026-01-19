;;; keybindings.el -*- lexical-binding: t; -*-
;;
;; All custom leader keybindings in one place.
;; This file is loaded at the end of config.el to ensure bindings are set last.

;;
;;; macOS specific

(when (featurep :system 'macos)
  ;; disable bindings clashing with Hammerspoon
  (map! "M-k" nil
        "M-j" nil)
  ;; fix for macOS UK keyboards (M-3 produces #)
  (map! (:map key-translation-map "M-3" "#")))

;;
;;; Leader bindings

(map!
 :leader

 ;; Top-level
 "§" #'other-frame
 "1" #'my/where-am-i

 ;; Windows
 (:prefix "w"
  :desc "Window switch" "w" #'persp-window-switch)

 ;; Buffers
 (:prefix "b"
  :desc "Kill buffer"              "k" #'my/kill-buffer
  :desc "Kill all buffers"         "K" #'my/kill-all-buffers
  :desc "Kill buffer"              "d" #'my/kill-buffer
  :desc "Kill all buffers"         "D" #'my/kill-all-buffers
  :desc "Kill buffers not current" "q" #'my/kill-all-buffers-except-current)

 ;; Files
 (:prefix "f"
  :desc "Find file in dotfiles" "." #'my/find-file-in-dotfiles)

 ;; Insert
 (:prefix "i"
  :desc "Insert date"      "d" #'my/insert-timestamp
  :desc "Insert date+time" "t" (lambda () (interactive) (my/insert-timestamp t))
  :desc "Insert email"     "E" #'my/insert-email)

 ;; Open
 (:prefix "o"
  :desc "Browse URL at point" "l" #'browse-url-at-point
  :desc "ChatGPT"             "c" #'my/chatgpt-open-prompt)

 ;; Search
 (:prefix "s"
  :desc "Search project (at point)" "w" #'my/vertico-search-project-symbol-at-point
  :desc "Search project"            "a" #'+vertico/project-search
  :desc "Repeat last search"        "." #'vertico-repeat)

 ;; Edit
 (:prefix ("e" . "edit")
  :desc "Yank from killring" "p" #'yank-from-kill-ring)

 ;; Toggle
 (:prefix "t"
  :desc "Truncate lines" "t" #'toggle-truncate-lines
  :desc "Imenu"          "i" #'imenu-list-smart-toggle)

 ;; Project
 (:prefix "p"
  :desc "Run Makefile target" "m" #'+make/run))

;;
;;; Module-specific bindings
;;
;; Keybindings that depend on personal modules being enabled.
;; Each section is guarded by `modulep!' to only load when the module is active.

;; Terminal (:smallwat3r terminal)
(when (modulep! :smallwat3r terminal)
  (map!
   :leader
   (:prefix "o"
    :desc "Terminal"                "1" #'my/terminal-here
    :desc "Vterm at root"           "T" #'+vterm/here
    :desc "Toggle vterm at root"    "t" #'+vterm/toggle
    :desc "Vterm at buffer"         "V" #'my/vterm/here-current-buffer
    :desc "Toggle vterm at buffer"  "v" #'my/vterm/toggle-current-buffer
    :desc "Tramp SSH conn"          "." #'my/open-remote-conn
    :desc "Term SSH conn"           "s" #'my/ssh-external))
  (map! :map vterm-mode-map
        :n "B" #'vterm-beginning-of-line
        :n "<return>" #'evil-insert-resume
        [remap delete-forward-char] #'vterm-send-delete
        :in "<M-backspace>" #'vterm-send-meta-backspace
        :n "<M-backspace>" #'vterm-send-meta-backspace
        :in "C-k" #'vterm-send-up
        :in "C-j" #'vterm-send-down
        "C-;" #'my/vterm-zsh-history-pick)
  (add-hook! 'vterm-mode-hook
    (defun my/vterm-dd-binding ()
      (evil-local-set-key 'normal "dd" (cmd! (vterm-send-C-c))))))

;; Google (:smallwat3r google)
(when (modulep! :smallwat3r google)
  (map!
   :leader
   (:prefix "o"
    :desc "Query google" "g" #'google-this)

   (:prefix ("T" . "translate")
    :desc "Translate query"    "q" #'google-translate-query-translate
    :desc "Translate at point" "t" #'google-translate-at-point
    :desc "Translate buffer"   "b" #'google-translate-buffer)

   (:prefix ("G" . "google")
    :desc "Query google"     "q" #'google-this
    :desc "Google this word" "w" #'google-this-word
    :desc "Google this line" "l" #'google-this-line)))

;; Password (:smallwat3r tools)
(when (modulep! :smallwat3r tools)
  (map!
   :leader
   (:prefix ("P" . "password")
    :desc "Open password-store buffer" "p" #'pass)))

;; Debug (:smallwat3r debug)
(when (modulep! :smallwat3r debug)
  (map!
   :leader
   (:prefix "t"
    :desc "Debug mode" "D" #'my-debug-mode)))

;; Claude (:smallwat3r claude)
(when (modulep! :smallwat3r claude)
  (map!
   :leader
   (:prefix ("r" . "AI")
    :desc "Claude chat"                 "c" #'my/claude-chat
    :desc "Claude chat at root"         "C" #'my/claude-chat-project-root
    :desc "New Claude chat"             "n" #'my/claude-new-chat
    :desc "New Claude chat at root"     "N" #'my/claude-new-chat-project-root
    :desc "New named chat"              "a" #'my/claude-new-chat-named
    :desc "New named chat at root"      "A" #'my/claude-new-chat-named-project-root
    :desc "Rename chat"                 "R" #'my/claude-rename-chat
    :desc "Switch Claude chat"          "s" #'my/claude-switch-chat
    :desc "Toggle Claude"               "t" #'my/claude-toggle
    :desc "Send region"                 "r" #'my/claude-send-region
    :desc "Send buffer"                 "b" #'my/claude-send-buffer
    :desc "Send region with context"    "x" #'my/claude-send-region-with-context
    :desc "Send region with prompt"     "p" #'my/claude-send-region-with-prompt)))

;; Evil (:smallwat3r evil-ext)
(when (modulep! :smallwat3r evil-ext)
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

  (after! evil-snipe
    (map! :map evil-snipe-parent-transient-map
          :g "j" #'evil-snipe-repeat
          :g "k" #'evil-snipe-repeat-reverse
          :g "n" #'evil-snipe-repeat         ; custom layout
          :g "a" #'evil-snipe-repeat-reverse)))
