;;; $DOOMDIR/+eshell.el -*- lexical-binding: t; -*-

(use-package! shrink-path
  :commands shrink-path-prompt)

(defun zz/eshell-current-git-branch ()
  "Get current branch name from repository."
  (let ((args '("symbolic-ref" "HEAD" "--short")))
    (with-temp-buffer
      (apply #'process-file "git" nil (list t nil) nil args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

(defun zz/eshell-prompt ()
  "Default Eshell prompt."
  (let ((base/dir (shrink-path-prompt default-directory))
        (base/branch (zz/eshell-current-git-branch)))
    (concat
     ; python venv
     (if (getenv "VIRTUAL_ENV")
         (let ((venv (file-name-nondirectory (getenv "VIRTUAL_ENV"))))
           (propertize (format "(%s) " venv) 'face 'default)))
     ; directory path
     (propertize (car base/dir) 'face 'font-lock-comment-face)
     (propertize (cdr base/dir) 'face 'default)
     ; git branch
     (if base/branch
         (propertize (format " \ue0a0 %s" base/branch) 'face 'default))
     ; user / super user
     (propertize (if (= (user-uid) 0) " # " " → ") 'face 'default))))

;; Remove the virtual env variable once the env has been deactivated, it will
;; get recreated once we reactivate the env. It's used in our eshell prompt
;; so we need to remove it when not in use.
(add-hook! 'pyvenv-post-deactivate-hooks (lambda () (setenv "VIRTUAL_ENV" nil)))

(after! eshell
  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 5000
        eshell-modify-global-environment t
        eshell-prompt-regexp "^.* [#→] "
        eshell-prompt-function #'zz/eshell-prompt)

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

(use-package! esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;;
;; Custom Eshell functions
;;

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
