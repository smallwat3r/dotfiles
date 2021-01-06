;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

(doom!
 :completion
 company           ; the ultimate code completion backend
 (ivy               ; a search engine for love and life
  +fuzzy
  +prescient)

 :ui
 deft              ; notational velocity for Emacs
 doom              ; what makes DOOM look the way it does
 doom-dashboard    ; a nifty splash screen for Emacs
 (emoji
  +unicode)        ; 🙂
 hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
 ophints           ; highlight the region an operation acts on
 (popup
  +defaults)   ; tame sudden yet inevitable temporary windows
 treemacs          ; a project drawer, like neotree but cooler
 vc-gutter         ; vcs diff in the fringe
 workspaces        ; tab emulation, persistence & separate workspaces
 zen               ; distraction-free coding or writing

 :editor
 (evil
  +everywhere)   ; come to the dark side, we have cookies
 file-templates    ; auto-snippets for empty files
 (fold
  +vimish-fold
  +evil-vimish-fold) ; (nigh) universal code folding
 format            ; automated prettiness
 ;;multiple-cursors  ; editing in many places at once
 snippets          ; my elves. They type so I don't have to
 word-wrap         ; soft wrapping with language-aware indent

 :emacs
 dired             ; making dired pretty [functional]
 electric          ; smarter, keyword-based electric-indent
 undo              ; persistent, smarter undo for your inevitable mistakes
 vc                ; version-control and Emacs, sitting in a tree

 :term
 eshell            ; the elisp shell that works everywhere
 vterm             ; the best terminal emulation in Emacs

 :checkers
 syntax            ; tasing you for every semicolon you forget
 (spell
  +aspell)        ; tasing you for misspelling mispelling
 ;;grammar           ; tasing grammar mistake every you make

 :tools
 docker
 (eval
  +overlay)     ; run code, run (also, repls)
 (lookup
  +dictionary
  +docsets)   ; navigate your code and its documentation
 lsp
 magit             ; a git porcelain for Emacs
 make              ; run make tasks from Emacs
 pass              ; password manager for nerds
 ;;pdf               ; pdf enhancements
 rgb               ; creating color strings

 :os
 (:if IS-MAC macos)   ; MacOS-specific commands
 tty                 ; improve the terminal Emacs experience

 :lang
 cc                ; C/C++/Obj-C madness
 emacs-lisp        ; drown in parentheses
 (go
  +lsp)         ; the hipster dialect
 json              ; At least it ain't XML
 (javascript
  +lsp)            ; all(hope(abandon(ye(who(enter(here))))))
 markdown          ; writing docs for people to ignore
 (org
  +journal)         ; organize your plain life in plain text
 (python
  +lsp)            ; beautiful is better than ugly
 rest              ; Emacs as a REST client
 (sh
  +lsp)             ; she sells {ba,z,fi}sh shells on the C xor
 yaml              ; JSON, but readable

 :email
 notmuch

 :app
 ;;(rss +org)        ; emacs as an RSS reader
 ;;twitter           ; twitter client https://twitter.com/vnought

 :config
 ;;literate
 (default
   +bindings
   +smartparens))
