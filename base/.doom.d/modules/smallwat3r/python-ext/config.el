;;; smallwat3r/python-ext/config.el -*- lexical-binding: t; -*-

(defvar my-python-line-length 88
  "Default Python line length for formatters.")

(defvar my-python-target-version "py310"
  "Target Python version for black formatter.")

(after! python
  ;; Disable annoying warnings about `python-shell-interpreter' readline support
  (setq python-shell-completion-native-enable nil)

  ;; Isort
  (after! py-isort
    (setq py-isort-options
          `("--trailing-comma" "--use-parentheses"
            ,(format "-l %s" my-python-line-length))))

  ;; Formatter
  (set-formatter! 'black
    `("black" "--quiet"
      "--line-length" ,(number-to-string my-python-line-length)
      "--target-version" ,my-python-target-version
      "-")
    :modes '(python-mode))


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

;; DAP (Debug Adapter Protocol)
(after! dap-mode
  (setq dap-python-debugger 'debugpy)
  (require 'dap-python))

;; PET (Python Executable Tracker)
;; Automatically finds and uses the correct Python executable for the project
;; (e.g. from virtualenv, poetry, pyenv). Also configures eglot and dap-mode
;; to use the correct Python.
;; doc: https://github.com/wyuenho/emacs-pet/
(use-package! pet
  :config
  (add-hook! 'python-mode-hook
    (pet-mode)
    (when-let ((python (pet-executable-find "python")))
      ;; Eglot/basedpyright
      (setq-local eglot-workspace-configuration
                  `(:basedpyright (:pythonPath ,python)))
      ;; DAP debugger
      (setq-local dap-python-executable python))))
