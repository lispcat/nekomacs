

;; no cover
;; (use-package listen)

;; emms extract metadata?
;; https://www.reddit.com/r/emacs/comments/981khz/emacs_music_player_with_emms/

;; TODO: 
(use-package emms
  :defer t
  :general
  (neko/leader-definer
    "e" '(:ignore t :which-key "wao")
    "e e" 'emms
    "e n" 'emms-next
    "e p" 'emms-previous
    "e s" 'emms-seek-to
    
    "e i" '(:ignore t :which-key "info")
    "e i i" 'emms-show
    "e i a" 'emms-show-all
    
    "e b" '(:ignore t :which-key "browse")
    "e b b" 'emms-browser
    "e b a" 'emms-browse-by-album
    "e b A" 'emms-browse-by-artist)
  
  :general-config
  (neko/leader-definer
    "e S" emms-playlist-sort-map)
  
  :config
  (emms-all)
  (require 'emms-player-mpd)

  ;; variables
  
  (setq emms-source-file-default-directory "~/Music/library/")
  
  ;; emms-player-mpv-parameters '("--no-audio-display=no"); broken
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  ;; sort by natural order
  (setq emms-playlist-sort-function #'emms-playlist-sort-by-natural-order)
  ;; ;; sort album by natural order
  ;; ;; (setq  emms-browser-album-sort-function #'emms-playlist-sort-by-natural-order)
  ;; this actually sorts by natural order upon adding
  (add-hook 'emms-playlist-source-inserted-hook #'emms-playlist-sort-by-natural-order)

  ;; backends
  
  (setq emms-player-list '(emms-player-mpd emms-player-mpv))

  ;; get info from mpd
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  ;; ? show current song when next song starts?
  (add-hook 'emms-player-started-hook #'emms-show)
  ;; connect to mpd
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "\~/Music/library")
  (emms-player-mpd-connect)

  ;; enable playerctl pausing

  ;; DISABLE LATER when using mpd-mpris service
  (require 'emms-mpris)
  (emms-mpris-enable) ;; (will make emacs hog mpris media playing active)

  ;; (setq emms-player-list '(emms-player-mpd))
  ;; (add-to-list 'emms-info-functions 'emms-info-mpd)
  ;; (add-to-list 'emms-player-list 'emms-player-mpd)

  ;; browser
  
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
		  (message "extracting: %s"
			   (format "ffmpeg -i %s -an -vcodec copy %s -y"
				   (shell-quote-argument song-path)
				   (shell-quote-argument cover-path))))))
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
      (message "No song currently playing")))

  ;; Hook to display album cover in Emacs when the track changes
  ;; (add-hook 'emms-player-started-hook 'emms-show-album-cover-in-emacs)
  )
