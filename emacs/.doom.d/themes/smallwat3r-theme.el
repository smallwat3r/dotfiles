;;; smallwat3r-theme.el -*- lexical-binding: t -*-

(deftheme smallwat3r "Personal minimalistic theme.")

(defgroup smallwat3r-theme nil
  "Smallwat3r theme."
  :group 'faces
  :prefix "smallwat3r-"
  :tag "Smallwat3r theme")

(custom-theme-set-faces
 'smallwat3r
 `(default ((t (:background "cornsilk" :foreground "black"))))
 `(company-tooltip ((t (:background "moccasin"))))
 `(font-lock-comment-face ((t (:foreground "Firebrick" :slant italic))))
 `(font-lock-doc-face ((t (:foreground "Firebrick" :slant italic))))
 `(region ((t (:background "wheat2" :extend nil))))
 `(highlight ((t (:background "DarkSlateGray1"))))
 `(lazy-highlight ((t (:background "gold2" :foreground "black"))))
 `(whitespace-tab ((t (:background "PaleTurquoise1"))))
 `(fringe ((t (:background unspecified))))
 `(magit-section-highlight ((t (:background unspecified))))
 `(magit-diff-context-highlight ((t (:background unspecified))))
 `(term-color-white ((t (:foreground "grey41"))))
 `(diredfl-dir-name ((t (:background unspecified :underline t))))
 `(markdown-header-face-1 ((t (:foreground "#0000FF"))))
 `(markdown-header-face-2 ((t (:foreground "#1F1FFF"))))
 `(markdown-header-face-3 ((t (:foreground "#4949FF"))))
 `(markdown-header-face-4 ((t (:foreground "#7879FF"))))
 `(markdown-header-face-5 ((t (:foreground "#A3A3FF"))))
 `(markdown-header-face-6 ((t (:foreground "#BFBFFF"))))
 `(markdown-inline-code-face ((t (:box (:line-width 2 :style released-button)
                                  :background "lightgrey"
                                  :foreground "DarkMagenta"))))
 `(markdown-pre-face ((t (:background "lightgrey" :foreground "black" :extend t))))
 `(markdown-language-keyword-face ((t (:foreground "red3"))))
 `(org-verbatim ((t (:inherit markdown-inline-code-face))))
 `(org-block ((t (:background "lightgrey" :foreground "black" :extend t))))
 `(org-block-begin-line ((t (:background unspecified :foreground "red3"))))
 `(org-block-end-line ((t (:background unspecified :foreground "red3"))))
 `(org-level-1 ((t (:foreground "#0000FF"))))
 `(org-level-2 ((t (:foreground "#1F1FFF"))))
 `(org-level-3 ((t (:foreground "#4949FF"))))
 `(org-level-4 ((t (:foreground "#7879FF"))))
 `(org-level-5 ((t (:foreground "#A3A3FF"))))
 `(org-level-6 ((t (:foreground "#BFBFFF"))))
 `(org-level-7 ((t (:foreground "DarkViolet"))))
 `(org-level-8 ((t (:foreground "SkyBlue2"))))
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
