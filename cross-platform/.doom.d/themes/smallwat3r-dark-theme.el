;;; smallwat3r-dark-theme.el --- A simple dark theme  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Matthieu Petiteau <mpetiteau.pro@gmail.com>

;; Homepage: https://github.com/smallwat3r/dotfiles/tree/master/cross-platform/.doom.d/themes/smallwat3r-dark-theme.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Licence: GPL-3.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple dark theme.

;;; Code:

(deftheme smallwat3r-dark "A simple dark theme.")

(defgroup smallwat3r-dark-theme nil
  "Smallwat3r dark theme."
  :group 'faces
  :prefix "smallwat3r-dark-"
  :tag "Smallwat3r dark theme")

(custom-theme-set-faces
 'smallwat3r-dark

 `(default ((t (:background "black" :foreground "white"))))

 `(lsp-headerline-breadcrumb-symbols-face ((t (:inherit font-lock-constant-face))))
 `(lsp-headerline-breadcrumb-path-face ((t (:inherit font-lock-constant-face))))

 `(success ((t (:foreground "green4" :weight bold))))
 `(warning ((t (:foreground "OrangeRed3" :weight bold))))
 `(error ((t (:foreground "firebrick2" :weight bold))))

 `(highlight-numbers-number ((t (:foreground "dark cyan"))))
 `(font-lock-doc-face ((t (:foreground "PaleGoldenrod"))))
 `(font-lock-comment-face ((t (:foreground "yellow3"))))
 `(font-lock-comment-delimiter-face ((t (:foreground "yellow3"))))
 `(font-lock-keyword-face ((t (:foreground "orchid"))))
 `(font-lock-string-face ((t (:foreground "PaleGoldenrod"))))

 `(makefile-targets ((t (:inherit font-lock-keyword-face))))
 `(nxml-element-local-name ((t (:inherit font-lock-keyword-face))))

 ;; Remove highlighting on some major programming faces. Keep it clean and simple.
 `(font-lock-function-name-face ((t (:foreground unspecified))))
 `(font-lock-variable-name-face ((t (:foreground unspecified))))
 `(font-lock-constant-face ((t (:foreground unspecified))))
 `(font-lock-builtin-face ((t (:foreground unspecified))))
 `(font-lock-type-face ((t (:foreground unspecified))))

 `(font-lock-regexp-grouping-backslash ((t :foreground "chartreuse4" :weight bold)))

 `(link ((t (:foreground "cyan4" :underline t))))

 `(region ((t (:background "grey10" :extend t))))

 `(highlight ((t (:background "SteelBlue4"))))
 `(lazy-highlight ((t (:background "dark goldenrod" :foreground "black" :weight bold))))

 `(whitespace-tab ((t (:background "PaleTurquoise4"))))

 `(show-paren-match ((t (:background "#c488ff" :foreground "black" :underline t :weight bold))))
 `(show-paren-mismatch ((t (:background "red4" :foreground "red" :weight bold))))

 `(fringe ((t (:background unspecified))))
 `(line-number ((t :foreground "grey12")))

 `(git-commit-summary ((t (:weight bold))))
 `(magit-diff-context-highlight ((t (:background unspecified))))

 `(term-color-white ((t (:foreground "grey90" :background "grey90"))))
 `(term-color-black ((t (:foreground "grey59" :background "grey59"))))
 `(term-color-yellow ((t (:foreground "yellow3" :background "yellow3"))))
 `(term-color-blue ((t (:foreground "SteelBlue2" :background "SteelBlue2"))))
 `(term-color-red ((t (:foreground "VioletRed3" :background "VioletRed3"))))
 `(term-color-cyan ((t (:foreground "cyan4" :background "cyan4"))))
 `(term-color-green ((t (:foreground "green3" :background "green3"))))
 `(term-color-magenta ((t (:foreground "magenta" :background "magenta"))))

 `(term-color-bright-white ((t (:inherit term-color-white))))
 `(term-color-bright-black ((t (:inherit term-color-black))))
 `(term-color-bright-yellow ((t (:inherit term-color-yellow))))
 `(term-color-bright-blue ((t (:inherit term-color-blue))))
 `(term-color-bright-red ((t (:inherit term-color-red))))
 `(term-color-bright-cyan ((t (:inherit term-color-cyan))))
 `(term-color-bright-green ((t (:inherit term-color-green))))
 `(term-color-bright-magenta ((t (:inherit term-color-magenta))))

 `(diredfl-dir-name ((t (:background unspecified :underline t))))

 `(org-block-begin-line ((t (:inherit font-lock-comment-face :background "gray20" :extend t))))
 `(org-block-end-line ((t (:inherit font-lock-comment-face :background "gray20" :extend t))))
 `(org-code ((t (:inherit fixed-pitch :background "gray10"))))
 `(org-block ((t (:inherit fixed-pitch :background "gray10" :extend t))))
 `(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 `(org-table ((t (:inherit fixed-pitch :foreground "gray41"))))
 `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 `(org-property-value ((t (:inherit fixed-pitch))) t)
 `(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold))))
 `(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))

 `(outline-1 ((t (:foreground "purple"))))
 `(outline-2 ((t (:foreground "VioletRed3"))))
 `(outline-3 ((t (:foreground "turquoise3"))))
 `(outline-4 ((t (:foreground "coral"))))
 `(outline-5 ((t (:foreground "firebrick"))))
 `(outline-6 ((t (:foreground "yellow green"))))
 `(outline-7 ((t (:foreground "pale green"))))
 `(outline-8 ((t (:foreground "medium slate blue"))))

 `(markdown-header-face-1 ((t (:inherit outline-1))))
 `(markdown-header-face-2 ((t (:inherit outline-2))))
 `(markdown-header-face-3 ((t (:inherit outline-3))))
 `(markdown-header-face-4 ((t (:inherit outline-4))))
 `(markdown-header-face-5 ((t (:inherit outline-5))))
 `(markdown-header-face-6 ((t (:inherit outline-6))))
 `(markdown-inline-code-face ((t :inherit org-block)))
 `(markdown-pre-face ((t (:inherit org-code :extend t))))
 `(markdown-language-keyword-face ((t (:inherit org-block-begin-line))))

 `(whitespace-space ((t (:inherit whitespace-newline))))
 `(whitespace-tabs ((t (:inherit whitespace-newline))))

 `(git-gutter-fr:added ((t (:background unspecified :foreground "green"))))
 `(git-gutter-fr:deleted ((t (:background unspecified :foreground "red"))))
 `(git-gutter-fr:modified ((t (:background unspecified :foreground "magenta"))))

 `(doom-modeline-buffer-file ((t (:foreground unspecified :weight bold))))
 `(doom-modeline-project-dir ((t (:foreground unspecified :weight bold))))
 `(doom-modeline-info ((t (:foreground unspecified :weight bold))))
 `(doom-modeline-highlight ((t (:foreground "magenta" :weight bold))))
 `(doom-modeline-evil-insert-state ((t (:foreground "green4" :weight bold))))
 `(doom-modeline-compilation ((t (:inherit warning :slant italic))))

 `(symbol-overlay-face-1 ((t (:foreground "black" :background "orchid2" :weight bold))))
 `(symbol-overlay-face-2 ((t (:foreground "black" :background "DarkOrange1" :weight bold))))
 `(symbol-overlay-face-3 ((t (:foreground "black" :background "chartreuse2" :weight bold))))
 `(symbol-overlay-face-4 ((t (:foreground "black" :background "turquoise2" :weight bold))))
 `(symbol-overlay-face-5 ((t (:foreground "black" :background "tomato" :weight bold))))
 `(symbol-overlay-face-6 ((t (:foreground "black" :background "peach puff" :weight bold))))
 `(symbol-overlay-face-7 ((t (:foreground "black" :background "medium violet red" :weight bold))))
 `(symbol-overlay-face-8 ((t (:foreground "black" :background "dark violet" :weight bold))))

 `(corfu-default ((t (:inherit default))))
 `(corfu-current ((t (:inherit highlight)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smallwat3r-dark)

;;; smallwat3r-dark-theme.el ends here
