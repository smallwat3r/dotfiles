;;; $DOOMDIR/+eshell.el -*- lexical-binding: t; -*-

(after! eshell
  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 5000)

  ;; List of eshell aliases
  (set-eshell-alias!
   "d" "dired $1"
   "clear" "clear-scrollback"
   "c" "clear-scrollback"
   "sl" "ls"
   "g" "git $*"
   "gs" "magit-status"
   "gc" "magit-commit"
   "gd" "magit-diff-unstaged"
   "gds" "magit-diff-staged"
   "venv" "pyvenv-activate $1"
   "deactivate" "pyvenv-deactivate"
   "qq" "exit"
   "..." "cd ../.."
   "...." "cd ../../.."
   "....." "cd ../../../.."
   "k" "kubectl $*"
   "kt" "kubetail $*"
   "kgn" "kubectl get namespaces"
   "gpg-pub-key" "gpg --armor --export mpetiteau.pro@gmail.com"
   "gpg-list-keys" "gpg --list-secret-keys --keyid-format LONG"
   "diskspace" "df -P -kHl"))

;; Provides a custom eshell prompt
(use-package! eshell-git-prompt
  :after eshell
  :commands eshell-git-prompt-use-theme
  :init (eshell-git-prompt-use-theme 'robbyrussell))

;; Autosuggestions
(use-package! esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(defun eshell/cr ()
  "cd into the repository root directory."
  (require 'magit)
  (eshell/cd (magit-toplevel)))

(defun eshell/dots ()
  "cd into my dotfiles directory."
  (eshell/cd "~/dotfiles"))

(defun eshell/e (&rest args)
  "Invoke `find-file' on the file.
\"e +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
(defun eshell/ef (filename &optional dir)
  "Searches for the first matching filename and loads it into a
file to edit."
  (let* ((files (eshell/f filename dir))
         (file (car (s-split "\n" files))))
    (find-file file)))
