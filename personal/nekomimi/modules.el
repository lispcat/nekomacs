;;; Userside modules config  -*- lexical-binding: t; -*-

;;;;;; Completion ;;;;;;

;; (+require 'nekomimi-vertico-meta)
(+require 'nekomimi-vertico)
(+require 'nekomimi-orderless)
(+require 'nekomimi-marginalia)
(+require 'nekomimi-embark)
(+require 'nekomimi-consult) ;; TODO: try disabling consult and see what some default keybinds like C-. & C-; do?
(+require 'nekomimi-corfu)
(+require 'nekomimi-cape)
(+require 'nekomimi-yasnippet)

;;;;;; Enhancements ;;;;;;

(+require 'nekomimi-enhancements)

;;;;;; Files ;;;;;;

(+require 'nekomimi-files)
(+require 'nekomimi-dired)
(+require 'nekomimi-file-tweaks)
(+require 'nekomimi-buffers)

;;;;;; Keybinds ;;;;;;

(+require 'nekomimi-meow-dvp)

;;;;;; Languages ;;;;;;

(+require 'nekomimi-languages)
(+require 'nekomimi-eglot)
(+require 'nekomimi-lisp)
(+require 'nekomimi-lisp-adv)
(+require 'nekomimi-scheme)

;;;;;; Org ;;;;;;

(+require 'nekomimi-org)
(+require 'nekomimi-org-agenda)
;; - what about if multiple use-package's in that file/module? force load?
;; - `use-package emacs` for misc configs? and just add `:after`?
;; - also create a `+with-eval-after-load-all` macro for convenience?

;;;;;; prettify ;;;;;;

(+require 'nekomimi-themes)
(+require 'nekomimi-transparency)

;;;;;; Programs ;;;;;;

(+require 'nekomimi-magit)
(+require 'nekomimi-vterm)

;;;;;; Special ;;;;;;

(+require 'nekomimi-server)

;;;;;; Tweaks ;;;;;;

(+require 'nekomimi-sane-defs)

;;;;;; UI ;;;;;;

(+require 'nekomimi-line)
(+require 'nekomimi-modeline)
(+require 'nekomimi-scroll)
(+require 'nekomimi-windows)
