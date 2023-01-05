;;; smallwat3r/scratch/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/scratch-rest-mode ()
  "Open a scratch buffer with restclient."
  (interactive)
  (scratch 'restclient-mode))
