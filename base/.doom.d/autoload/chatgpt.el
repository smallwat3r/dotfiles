;;; $DOOMDIR/autoload/chatgpt.el -*- lexical-binding: t; -*-
;;
;; ChatGPT integration via browser.

;;;###autoload
(defun my/chatgpt-open-prompt ()
  "Open a popup buffer to compose a ChatGPT prompt.
Write your prompt, then press C-c C-c to open in browser, or C-c C-k to cancel."
  (interactive)
  (let* ((buf (get-buffer-create "*ChatGPT Prompt*"))
         (win (display-buffer
               buf
               '((display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25)))))
    (select-window win)
    (with-current-buffer buf
      (erase-buffer)
      (my-chatgpt-prompt-mode))))

;;;###autoload
(define-derived-mode my-chatgpt-prompt-mode text-mode "ChatGPT-Prompt"
  "Mode for composing ChatGPT prompts.
Keybindings:
  C-c C-c  Send prompt to ChatGPT in browser
  C-c C-k  Cancel and close the prompt buffer"
  (local-set-key (kbd "C-c C-c")
                 (lambda ()
                   (interactive)
                   (let* ((question (buffer-substring-no-properties
                                     (point-min) (point-max)))
                          (encoded (url-hexify-string question))
                          (url (concat "https://chatgpt.com/?prompt=" encoded)))
                     (browse-url url)
                     (quit-window t))))
  (local-set-key (kbd "C-c C-k") #'quit-window))
