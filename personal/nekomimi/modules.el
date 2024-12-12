;;; Userside modules config  -*- lexical-binding: t; -*-

;;;;;; Completion ;;;;;;

;; (+require 'neko-meta-vertico)
(+require 'neko-vertico)
(+require 'neko-orderless)
(+require 'neko-marginalia)
(+require 'neko-embark)
(+require 'neko-consult) ;; TODO: try disabling consult and see what some default keybinds like C-. & C-; do?
(+require 'neko-corfu)
(+require 'neko-cape)
(+require 'neko-yasnippet)

;;;;;; Enhancements ;;;;;;

(+require 'neko-enhancements)

;;;;;; Files ;;;;;;

(+require 'neko-files)
(+require 'neko-dired)
(+require 'neko-file-tweaks)
(+require 'neko-buffers)

;;;;;; Keybinds ;;;;;;

(+require 'neko-meow-dvp)

;;;;;; Languages ;;;;;;

(+require 'neko-languages)
(+require 'neko-eglot)
(+require 'neko-lisp)
(+require 'neko-lisp-adv)
(+require 'neko-scheme)

;;;;;; Org ;;;;;;

(+require 'neko-org)
(+require 'neko-org-agenda)
;; - what about if multiple use-package's in that file/module? force load?
;; - `use-package emacs` for misc configs? and just add `:after`?
;; - also create a `+with-eval-after-load-all` macro for convenience?

;;;;;; prettify ;;;;;;

(+require 'neko-themes)
(+require 'neko-transparency)

;;;;;; Programs ;;;;;;

(+require 'neko-magit)
(+require 'neko-vterm)

;;;;;; Special ;;;;;;

(+require 'neko-server)

;;;;;; Tweaks ;;;;;;

(+require 'neko-sane-defs)

;;;;;; UI ;;;;;;

(+require 'neko-line)
(+require 'neko-modeline)
(+require 'neko-scroll)
(+require 'neko-windows)
