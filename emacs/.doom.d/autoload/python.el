;;; $DOOMDIR/autoload/python.el -*- lexical-binding: t; -*-

(defvar my-python-virtual-env-directory ".venv"
  "Common name used for python virtual env directories.")

(defun my/venv--locate-python-path ()
  "Look for the closest Python virtual envs in the workspace."
  (when-let (venv-base-directory (locate-dominating-file
                                  default-directory
                                  my-python-virtual-env-directory))
    (concat venv-base-directory my-python-virtual-env-directory)))

(defun my/venv--locate-python-executable ()
  "Look for a Python executable from the closest virtual env."
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
