;;; $DOOMDIR/autoload/music.el -*- lexical-binding: t; -*-

;; Wrapper around pytunes to manage Apple music
;;
;; Install pytunes with:
;;   git clone https://github.com/hile/pytunes
;;   pip3 install file:///<fullpath to pytunes repo>

;;;###autoload
(defun my/music-info ()
  "Display current track information."
  (interactive)
  (let ((raw (process-lines "pytunes" "info")))
    (message "Listening to: %s (%sm) [%s (%s)] by %s"
             (string-trim (string-remove-prefix "Title" (nth 3 raw)))
             (string-trim (string-remove-prefix "Length" (nth 6 raw)))
             (string-trim (string-remove-prefix "Album" (nth 2 raw)))
             (string-trim (string-remove-prefix "Year" (nth 7 raw)))
             (string-trim (string-remove-prefix "Artist" (nth 1 raw))))))

;;;###autoload
(defun my/music-play-pause-toggle ()
  "Toggle play/pause current track."
  (interactive)
  (shell-command "pytunes play")
  (my/music-info))

;;;###autoload
(defun my/music-play-next ()
  "Play next track."
  (interactive)
  (shell-command "pytunes next")
  (my/music-info))

;;;###autoload
(defun my/music-play-previous ()
  "Play previous track."
  (interactive)
  (shell-command "pytunes previous")
  (my/music-info))
