;;; smallwat3r-theme.el -*- lexical-binding: t -*-

(deftheme smallwat3r "Personal minimalistic theme.")

(defgroup smallwat3r-theme nil
  "Smallwat3r theme."
  :group 'faces
  :prefix "smallwat3r-"
  :tag "Smallwat3r theme")

(custom-theme-set-faces
 'smallwat3r
 `(default ((t (:background "antique white" :foreground "black"))))
 `(highlight-numbers-number ((t (:foreground "dark cyan"))))
 `(font-lock-comment-face ((t (:foreground "Firebrick" :slant italic))))
 `(font-lock-doc-face ((t (:foreground "Firebrick" :slant italic))))
 `(font-lock-regexp-grouping-backslash ((t :foreground "chartreuse4" :weight bold)))
 `(region ((t (:background "wheat2" :extend nil))))
 `(highlight ((t (:background "DarkSlateGray1"))))
 `(lazy-highlight ((t (:background "gold2"))))
 `(whitespace-tab ((t (:background "PaleTurquoise1"))))
 `(show-paren-match ((t (:background "#c488ff" :foreground "black" :underline t :weight bold))))
 `(show-paren-mismatch ((t (:background "red4" :foreground "red" :weight bold))))
 `(fringe ((t (:background unspecified))))
 `(magit-section-highlight ((t (:background unspecified))))
 `(magit-diff-context-highlight ((t (:background unspecified))))
 `(term-color-white ((t (:foreground "grey41"))))
 `(diredfl-dir-name ((t (:background unspecified :underline t))))
 `(org-code ((t (:foreground "gray41" :inherit fixed-pitch))))
 `(org-block ((t (:foreground "gray41" :inherit fixed-pitch))))
 `(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 `(org-table ((t (:inherit fixed-pitch :foreground "gray41"))))
 `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 `(org-property-value ((t (:inherit fixed-pitch))) t)
 `(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 `(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 `(outline-1 ((t (:foreground "#0000ff"))))
 `(outline-2 ((t (:foreground "#1f1fff"))))
 `(outline-3 ((t (:foreground "#4949ff"))))
 `(outline-4 ((t (:foreground "#7879ff"))))
 `(outline-5 ((t (:foreground "#a3a3ff"))))
 `(outline-6 ((t (:foreground "#bfbfff"))))
 `(outline-7 ((t (:foreground "DarkViolet"))))
 `(outline-8 ((t (:foreground "SkyBlue2"))))
 `(markdown-header-face-1 ((t (:inherit outline-1))))
 `(markdown-header-face-2 ((t (:inherit outline-2))))
 `(markdown-header-face-3 ((t (:inherit outline-3))))
 `(markdown-header-face-4 ((t (:inherit outline-4))))
 `(markdown-header-face-5 ((t (:inherit outline-5))))
 `(markdown-header-face-6 ((t (:inherit outline-6))))
 `(markdown-inline-code-face ((t :inherit org-block)))
 `(markdown-pre-face ((t (:inherit org-code))))
 `(markdown-language-keyword-face ((t (:inherit org-block-begin-line))))
 ;; I like to keep my editor clean and simple. De-activate syntax highlighting
 ;; on some major programming related faces such as variables or functions, as
 ;; I don't think having lots of colors helps with focus and readability.
 `(font-lock-function-name-face ((t (:foreground unspecified :weight normal))))
 `(font-lock-variable-name-face ((t (:foreground unspecified :weight normal))))
 `(font-lock-constant-face ((t (:foreground unspecified :weight normal))))
 `(font-lock-builtin-face ((t (:foreground unspecified :weight normal))))
 `(font-lock-type-face ((t (:foreground unspecified :weight normal)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smallwat3r)
