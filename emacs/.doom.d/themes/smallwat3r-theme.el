;;; smallwat3r-theme.el -*- lexical-binding: t -*-

(deftheme smallwat3r "Personal minimalistic theme.")

(defgroup smallwat3r-theme nil
  "Smallwat3r theme."
  :group 'faces
  :prefix "smallwat3r-"
  :tag "Smallwat3r theme")

(custom-theme-set-faces
 'smallwat3r
 `(default ((t (:background "PaleTurquoise" :foreground "black"))))
 `(region ((t (:background "wheat2"))))
 `(highlight ((t (:background "DarkSlateGray1"))))
 `(lazy-highlight ((t (:background "gold2" :foreground "black"))))
 `(whitespace-tab ((t (:background "PaleTurquoise1"))))
 `(fringe ((t (:background unspecified))))

 `(magit-section-highlight ((t (:background unspecified))))
 `(magit-diff-context-highlight ((t (:background unspecified))))

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
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background nil)))))))

(provide-theme 'smallwat3r)
