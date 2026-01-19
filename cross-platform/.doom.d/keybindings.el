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
    :desc "Term SSH conn"           "s" #'my/ssh-external)))

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
