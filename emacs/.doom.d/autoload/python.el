;;; $DOOMDIR/autoload/python.el -*- lexical-binding: t; -*-

(defun my/venv--locate-python-path ()
  "Look for the closest Python virtual environments in the workspace."
  (when-let (venv-base-directory (locate-dominating-file default-directory "env/"))
    (concat venv-base-directory "env")))

(defun my/venv--locate-python-executable ()
  "Look for a Python executable from the closest virtual environment."
  (when-let (venv-path (my/venv--locate-python-path))
    (executable-find (f-expand "bin/python" venv-path))))

;;;###autoload
(defun my/deactivate-python-venv ()
  "Deactivate Python virtual environment."
  (interactive)
  (pyvenv-deactivate)
  (message "Venv deactivated"))

;;;###autoload
(defun my/activate-closest-python-venv ()
  "Activate the closest Python virtual environment."
  (interactive)
  (if-let (venv-path-python (my/venv--locate-python-path))
      (let ((venv-path venv-path-python))
        (pyvenv-activate venv-path)
        (message "Activated venv `%s'" venv-path))
    (message "Couldn't find any available venv")))
