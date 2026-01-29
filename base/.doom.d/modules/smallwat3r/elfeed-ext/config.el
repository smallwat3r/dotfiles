;;; smallwat3r/elfeed-ext/config.el -*- lexical-binding: t; -*-

(after! elfeed
  (setq elfeed-search-filter "@1-month-ago")

  ;; Tag faces
  (defface my-github-elfeed-entry-face '((t :foreground "cyan4")) "Github entries.")
  (defface my-python-elfeed-entry-face '((t :foreground "IndianRed4")) "Python entries.")
  (defface my-emacs-elfeed-entry-face '((t :foreground "purple")) "Emacs entries.")

  (dolist (entry '((github my-github-elfeed-entry-face)
                   (python my-python-elfeed-entry-face)
                   (emacs my-emacs-elfeed-entry-face)))
    (cl-pushnew entry elfeed-search-face-alist :test #'equal))

  ;; Feeds
  (defvar my-elfeed-feed-titles
    '(("doomemacs/doomemacs/commits" . "Doom Emacs")
      ("smallwat3r.private" . "Github"))
    "Alist mapping URL substrings to display titles.")

  (setq elfeed-feeds
        '(("https://www.reddit.com/r/emacs.rss" reddit emacs)
          ("https://github.com/doomemacs/doomemacs/commits/master.atom" emacs doom)
          ("https://realpython.com/atom.xml?format=xml" python)
          ("http://feeds.feedburner.com/PythonInsider" python)))

  ;; Private Github feed from pass
  (when-let ((token (auth-source-pass-get 'secret "github/rss/token")))
    (add-to-list 'elfeed-feeds
                 (list (format "https://github.com/smallwat3r.private.atom?token=%s" token)
                       'github 'perso)))

  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "github.com/smallwat3r.private"
                                :add '(github perso)))

  ;; Custom entry display with nicer feed titles
  (defun my/elfeed-search-print-entry (entry)
    "Print ENTRY with custom feed titles."
    (let ((orig-feed-title (symbol-function 'elfeed-feed-title)))
      (cl-letf (((symbol-function 'elfeed-feed-title)
                 (lambda (feed)
                   (let ((url (elfeed-feed-url feed)))
                     (or (cdr (cl-find-if (lambda (x) (string-match-p (car x) url))
                                          my-elfeed-feed-titles))
                         (funcall orig-feed-title feed))))))
        (elfeed-search-print-entry--default entry))))

  (setq elfeed-search-print-entry-function #'my/elfeed-search-print-entry))
