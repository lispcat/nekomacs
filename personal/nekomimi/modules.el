;;; Userside modules config  -*- lexical-binding: t; -*-

;;;;;; Completion ;;;;;;

(+require 'neko-vertico)
(+require 'neko-vertico-cape)
(+require 'neko-vertico-consult) ;; TODO: try disabling consult and see what some default keybinds like C-. & C-; do?
(+require 'neko-vertico-corfu)
(+require 'neko-vertico-embark)
(+require 'neko-vertico-marginalia)
(+require 'neko-vertico-orderless)
(+require 'neko-vertico-yasnippet)

;;;;;; Enhancements ;;;;;;

(+require 'neko-enhancements)

;;;;;; Files ;;;;;;

(+require 'neko-files)
(+require 'neko-dired)
(+require 'neko-file-tweaks)
(+require 'neko-buffers)

;;;;;; IDE ;;;;;;

(+require 'neko-ide)
(+require 'neko-ide-lsp-mode)

;;;;;; Keybinds ;;;;;;

(+require 'neko-meow-dvp)
;; (+require 'neko-meow-qwerty) ;; for meow qwerty binds

;;;;;; Languages ;;;;;;

(+require 'neko-lang)
(+require 'neko-lang-elisp)
(+require 'neko-lang-lisp-adv)
(+require 'neko-lang-scheme)
(+require 'neko-lang-rust)

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
;; (+require 'neko-vterm) ; obsolete
(+require 'neko-eat)

;;;;;; Special ;;;;;;

(+require 'neko-server)

;;;;;; Tweaks ;;;;;;

(+require 'neko-sane-defs)

;;;;;; UI ;;;;;;

(+require 'neko-line)
(+require 'neko-modeline)
(+require 'neko-scroll)
(+require 'neko-windows)

