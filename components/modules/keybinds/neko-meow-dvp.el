

;;; Personal DVP configuration

(+require 'neko-meow)

(setq meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
(meow-motion-overwrite-define-key
 ;; custom keybinding for motion state
 '("<escape>" . ignore)
 '("t" . "p")	 ; improved solution? (access Motion "t" with "SPC t")
 )
(meow-leader-define-key
 '("t" . "H-t")
 ;; '("p" . "H-p")
 ;; '("u" . ctl-x-map)
 '("1" . meow-digit-argument)
 '("2" . meow-digit-argument)
 '("3" . meow-digit-argument)
 '("4" . meow-digit-argument)
 '("5" . meow-digit-argument)
 '("6" . meow-digit-argument)
 '("7" . meow-digit-argument)
 '("8" . meow-digit-argument)
 '("9" . meow-digit-argument)
 '("0" . meow-digit-argument)
 '("/" . meow-keypad-describe-key)
 '("?" . meow-cheatsheet))
(meow-normal-define-key
 ;; make S-<num> easier to hit with DVP by using symbols.
 '("*" . meow-expand-0)
 '("=" . meow-expand-9)
 '("!" . meow-expand-8)
 '("[" . meow-expand-7)
 '("]" . meow-expand-6)
 '("{" . meow-expand-5)
 '("+" . meow-expand-4)
 '("}" . meow-expand-3)
 '(")" . meow-expand-2)
 '("(" . meow-expand-1)
 '("1" . digit-argument)
 '("2" . digit-argument)
 '("3" . digit-argument)
 '("4" . digit-argument)
 '("5" . digit-argument)
 '("6" . digit-argument)
 '("7" . digit-argument)
 '("8" . digit-argument)
 '("9" . digit-argument)
 '("0" . digit-argument)
 ;; symbols
 '("-" . negative-argument)
 '(";" . meow-reverse)
 '(":" . meow-goto-line) ;; moved from "Q" and "E"
 '("," . meow-inner-of-thing)
 '("." . meow-bounds-of-thing)
 '("<" . meow-beginning-of-thing)
 '(">" . meow-end-of-thing)
 ;; basic letters
 '("a" . meow-append)
 '("A" . meow-open-below)
 '("b" . meow-back-word)
 '("B" . meow-back-symbol)
 '("c" . meow-change)
 ;; '("d" . ri/meow-delete-or-kill)
 '("d" . meow-delete) ; i want "d" to delete char after meow-prev/next-word, so dont use former
 '("D" . meow-backward-delete)
 '("e" . meow-line)
 ;; '("E" . meow-goto-line) ;; removed, since ":" for it works
 '("f" . meow-find)
 '("F" . meow-search) ;; moved from "s" ("s" is used for movement)
 '("g" . meow-cancel-selection)
 '("G" . meow-grab)
 ;; H Directional key moved to the bottom
 '("i" . meow-insert)
 '("I" . meow-open-above)
 '("j" . meow-join)
 '("k" . meow-kill)
 '("l" . meow-till)
 ;; '("m" . meow-mark-word) ;; swap with w, next-word (because "b"/"m" is easy for mvmnt)
 ;; '("M" . meow-mark-symbol) ;; swap with W, next-symbol (because "b"/"m" is easy for mvmnt)
 '("m" . meow-next-word)   ;; moved from "w", mark-word
 '("M" . meow-next-symbol) ;; moved from "W", mark-symbol
 ;; N Directional key moved to the bottom
 '("o" . meow-block)
 '("O" . meow-to-block)
 '("p" . meow-prev)
 '("P" . meow-prev-expand)
 '("q" . meow-quit)
 '("Q" . ri/quit-temp-window)
 ;; '("Q" . meow-goto-line) ;; move to " : "
 '("r" . meow-replace)
 '("R" . meow-swap-grab)
 ;; '("s" . meow-search) ;; move to F, replace with directional keys
 ;; S Directional key moved to the bottom
 ;; T Directional key moved to the bottom
 '("u" . meow-undo)
 '("U" . meow-undo-in-selection)
 '("v" . meow-visit)
 ;; '("w" . meow-next-word) ;; swap with m, mark-word/symbol
 ;; '("W" . meow-next-symbol)
 '("w" . meow-mark-word)   ;; moved from "m", mark-word
 '("W" . meow-mark-symbol) ;; moved from "M", mark-symbol
 '("x" . meow-save)
 '("X" . meow-sync-grab)
 '("y" . meow-yank)
 '("z" . meow-pop-selection)
 '("'" . repeat)
 '("/" . ri/scroll-down-half-page) ;; new keys
 '("?" . ri/scroll-up-half-page)   ;; new keys
 '("<escape>" .  keyboard-escape-quit)

 ;; Directional keys:

 ;; <-  ^  v  ->
 '("h" . meow-left)
 '("H" . meow-left-expand)
 '("t" . meow-prev)
 '("T" . meow-prev-expand)
 '("n" . meow-next)
 '("N" . meow-next-expand)
 '("s" . meow-right)
 '("S" . meow-right-expand)

 ;; ^  <-  v  ->
 ;; '("h" . meow-prev)
 ;; '("H" . meow-prev-expand)
 ;; '("t" . meow-left)
 ;; '("T" . meow-left-expand)
 ;; '("n" . meow-next)
 ;; '("N" . meow-next-expand)
 ;; '("s" . meow-right)
 ;; '("S" . meow-right-expand)

 ;; ^  /  <-  ->  v
 ;; '("h" . meow-left)
 ;; '("H" . meow-left-expand)
 ;; '("t" . meow-right)
 ;; '("T" . meow-right-expand)
 ;; '("n" . meow-prev)
 ;; '("N" . meow-prev-expand)
 )

(meow-global-mode 1)


(provide 'neko-meow-dvp)
