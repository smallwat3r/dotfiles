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
  :desc "Browse URL at point" "l" #'browse-url-at-point)

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
    :desc "Terminal"             "1" #'my/terminal-here
    :desc "Eat at root"          "T" #'my/eat/here
    :desc "Toggle eat at root"   "t" #'my/eat/toggle
    :desc "Eat at buffer"        "V" #'my/eat/here-current-buffer
    :desc "Toggle eat at buffer" "v" #'my/eat/toggle-current-buffer
    :desc "Tramp SSH conn"       "." #'my/open-remote-conn
    :desc "Term SSH conn"        "s" #'my/ssh-external))
  (map! :map eat-mode-map
        :n "B" #'beginning-of-line
        :n "E" #'end-of-line
        :n "0" #'beginning-of-line
        :n "$" #'end-of-line
        :n "G" #'end-of-buffer
        :n "gg" #'beginning-of-buffer
        :n "C-u" #'evil-scroll-up
        :n "C-d" #'evil-scroll-down
        :n "RET" #'evil-insert-state
        :ni "C-," #'my/eat-zsh-history-pick)
  (after! eat
    (evil-define-key 'normal eat-mode-map "dd" #'my/eat-interrupt)))

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
    :desc "Claude chat"              "c" #'claude-code
    :desc "Claude continue"          "C" #'claude-code-continue
    :desc "Claude resume"            "R" #'claude-code-resume
    :desc "New instance"             "n" #'claude-code-new-instance
    :desc "Start in directory"       "d" #'claude-code-start-in-directory
    :desc "Switch buffer"            "s" #'claude-code-switch-to-buffer
    :desc "Toggle Claude"            "t" #'claude-code-toggle
    :desc "Send region"              "r" #'claude-code-send-region
    :desc "Send command"             "p" #'claude-code-send-command
    :desc "Send with context"        "x" #'claude-code-send-command-with-context
    :desc "Send buffer file"         "b" #'claude-code-send-buffer-file
    :desc "Fix error at point"       "e" #'claude-code-fix-error-at-point
    :desc "Cycle mode"               "m" #'claude-code-cycle-mode
    :desc "Transient menu"           "M" #'claude-code-transient
    :desc "Kill session"             "k" #'claude-code-kill)))

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
