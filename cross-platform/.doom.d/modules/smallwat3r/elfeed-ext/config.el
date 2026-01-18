;;; smallwat3r/elfeed-ext/config.el -*- lexical-binding: t; -*-

;; Elfeed, web feed reader (RSS)
;; doc: https://github.com/skeeto/elfeed
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago")

  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "github.com/smallwat3r.private"
                                :add '(github perso)))

  (defface my-github-elfeed-entry-face '((t :foreground "cyan4"))
    "Face for a Github related Elfeed entry.")

  (defface my-python-elfeed-entry-face '((t :foreground "IndianRed4"))
    "Face for a Python related Elfeed entry.")

  (defface my-emacs-elfeed-entry-face '((t :foreground "purple"))
    "Face for an Emacs related Elfeed entry.")

  (cl-pushnew '(github my-github-elfeed-entry-face)
              elfeed-search-face-alist :test #'equal)
  (cl-pushnew '(python my-python-elfeed-entry-face)
              elfeed-search-face-alist :test #'equal)
  (cl-pushnew '(emacs my-emacs-elfeed-entry-face)
              elfeed-search-face-alist :test #'equal)

  (setq elfeed-feeds
        '(("https://www.reddit.com/r/emacs.rss" reddit emacs)
          ("https://github.com/doomemacs/doomemacs/commits/master.atom" emacs doom)
          ("https://realpython.com/atom.xml?format=xml" python)
          ("http://feeds.feedburner.com/PythonInsider" python)))

  ;; add private Github RSS feed, using token from pass.
  (let ((token (auth-source-pass-get 'secret "github/rss/token")))
    (when token
      (setq my-github-rss-feed
            (format "https://github.com/smallwat3r.private.atom?token=%s" token))
      (add-to-list 'elfeed-feeds (list my-github-rss-feed))))

  (defconst my-elfeed-doom-feed-url
    "https://github.com/doomemacs/doomemacs/commits/master.atom")

  ;; Custom print function: same as default, but with nicer feed titles.
  (defun my/elfeed-search-print-entry (entry)
    "Print ENTRY to the Elfeed search buffer with custom feed titles."
    (let* ((orig-feed-title (symbol-function 'elfeed-feed-title)))
      (cl-letf (((symbol-function 'elfeed-feed-title)
                 (lambda (feed)
                   (let ((url (elfeed-feed-url feed)))
                     (cond
                      ((and (boundp 'my-github-rss-feed)
                            my-github-rss-feed
                            (string= url my-github-rss-feed))
                       "Github feed")
                      ((string= url my-elfeed-doom-feed-url)
                       "Doom Emacs commits")
                      (t
                       (funcall orig-feed-title feed)))))))
        (elfeed-search-print-entry--default entry)))

  (setq elfeed-search-print-entry-function #'my/elfeed-search-print-entry)))
