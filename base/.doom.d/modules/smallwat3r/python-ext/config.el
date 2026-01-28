;;; smallwat3r/python-ext/config.el -*- lexical-binding: t; -*-

;; Python
(after! python
  (defvar my-default-python-line-length 88
    "Default python line length.")

  ;; Disable annoying warnings about `python-shell-interpreter' readline
  ;; support.
  (setq python-shell-completion-native-enable nil)

  ;; Isort
  (after! py-isort
    (setq py-isort-options '("--trailing-comma" "--use-parentheses"))
    (add-to-list 'py-isort-options (format "-l %s" my-default-python-line-length)))

  ;; Formatter
  (set-formatter! 'black
    '("black"
      "--quiet"
      "--line-length" (format "%s" my-default-python-line-length)
      "--target-version" "py310"
      "-")  ; apply in file changes
    :modes '(python-mode))

  ;; Debugger
  (after! dap-mode
    (setq dap-python-debugger 'debugpy))

  (defun my/python-toggle-fstring ()
    "Toggle f-string prefix on the current Python string literal."
    (interactive)
    (let* ((ppss (syntax-ppss))
           (in-string (nth 3 ppss))
           (string-start (nth 8 ppss))) ; position of opening quote
      (when in-string
        (save-excursion
          (goto-char string-start)
          (cond
           ;; immediate prefix char is f/F, remove it: f"..." -> "..."
           ((memq (char-before string-start) '(?f ?F))
            (delete-char -1))
           ;; combined prefix like rf"/fr" where the f is just before that
           ;; e.g. rf"..." or rf'...' or rf"""..."""
           ((and (> string-start 1)
                 (memq (char-before (1- string-start)) '(?f ?F))
                 (memq (char-before string-start) '(?r ?R ?b ?B ?u ?U)))
            (goto-char (1- string-start))
            (delete-char -1))
           ;; no f-prefix, add it
           (t
            (goto-char string-start)
            (insert "f")))))))

  (after! lookup
    (add-to-list '+lookup-provider-url-alist
                 '("Python Docs" "https://docs.python.org/3/search.html?q=%s"))))

;; PET (Python Executable Tracker)
;; Automatically finds and uses the correct Python executable for the project
;; (e.g. from virtualenv, poetry, pyenv).
;; doc: https://github.com/wyuenho/emacs-pet/
(use-package! pet
  :config
  (add-hook 'python-mode-hook 'pet-mode -10))
