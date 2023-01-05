;;; $DOOMDIR/autoload/python.el -*- lexical-binding: t; -*-

(defvar my-python-virtual-env-directory ".venv"
  "Common name used for python virtual env directories.")

(defun my/venv--locate-python-path ()
  "Look for the closest Python virtual env in the workspace."
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
  (setq lsp-pyright-venv-directory nil
        lsp-pyright-venv-path nil
        lsp-pyright-python-executable-cmd "python3"
        python-shell-interpreter "python3")
  (pyvenv-deactivate)
  (message "Venv deactivated"))

;;;###autoload
(defun my/activate-closest-python-venv ()
  "Activate the closest Python virtual environment."
  (interactive)
  (if-let (venv-path-python (my/venv--locate-python-path))
      (let* ((venv-path venv-path-python)
             (python-executable (concat venv-path "/bin/python")))
        (setq lsp-pyright-venv-directory my-python-virtual-env-directory
              lsp-pyright-venv-path venv-path
              lsp-pyright-python-executable-cmd python-executable
              python-shell-interpreter python-executable)
        (pyvenv-activate venv-path)
        (message "Activated venv `%s'" venv-path))
    (message "Couldn't find any available venv")))
