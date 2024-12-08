
(global-set-key (kbd "M-p") (kbd "M-- 1 C-v"))
(global-set-key (kbd "M-n") (kbd "M-- 1 M-v"))

;; no cover
;; (use-package listen)

;; TODO: 
(use-package emms
  :config
  (require 'emms-setup)
  ;; (require 'emms-player-mpd)
  ;; (require 'emms-browser)
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
	;; emms-info-functions '(emms-info-exiftool) ; lots o lag
	emms-source-file-default-directory "~/Music/Library/"
	;; emms-player-mpv-parameters '("--no-audio-display=no"); broken
	emms-browser-covers #'emms-browser-cache-thumbnail-async)
  ;; (setq emms-player-list '(emms-player-mpd))
  ;; (add-to-list 'emms-info-functions 'emms-info-mpd)
  ;; (add-to-list 'emms-player-list 'emms-player-mpd)

  ;; TODO: add this function to emms-info-functions (hard to implement?)
  ;; (instead make my own function that runs ffprobe and gets info? might be better)
  (defun my/emms-show-album-cover-in-emacs ()
    (interactive)
    (if-let ((track (emms-playlist-current-selected-track))
	     (song-path (emms-track-get track 'name))
	     (cover-path "/tmp/emms-album-cover.jpg")) ;; is jpg fine?
	(if (not (file-exists-p song-path))
	    (message "Error: cannot find path to currently playing song")
	  (when (file-exists-p cover-path)
	    (delete-file cover-path))
	  (let ((exit-code
		 (shell-command
		  (format "ffmpeg -i %s -an -vcodec copy %s -y"
			  (shell-quote-argument song-path)
			  (shell-quote-argument cover-path)))))
	    (cond ((/= exit-code 0)
		   (message "Error: ffmpeg cover extraction failed with code %s"
			    exit-code))
		  ((file-exists-p cover-path)
		   (with-current-buffer (get-buffer-create "*Album Cover*")
		     (erase-buffer)
		     (insert-image (create-image cover-path))
		     (pop-to-buffer (current-buffer))))
		  (t
		   (message "Error: ffmpeg cover at cover-path not found.")))))
      (message "No song currently playing"))))

;; Hook to display album cover in Emacs when the track changes
;; (add-hook 'emms-player-started-hook 'emms-show-album-cover-in-emacs)
