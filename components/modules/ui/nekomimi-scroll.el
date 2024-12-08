

;;; Improve scroll

(use-package emacs
  :custom
  ;; (auto-window-vscroll nil) ; TODO: what does this do?
  (scroll-preserve-screen-position t) ; keep point in same position while scrolling
  (scroll-conservatively 101) ; dont move cursor to center while scrolling
  (scroll-margin 2)	      ; scroll margin of one line
  (mouse-wheel-scroll-amount
   '(2					    ; faster vscroll speed
     ((shift) . hscroll)		    ; S-<scroll> for hscroll
     ((meta) . nil)			    ; M-<scroll> for PgUp/PgDn
     ((control) . text-scale)		    ; C-<scroll> for zoom
     ((control meta) . global-text-scale))) ; C-M-<scroll> for global zoom
  (mouse-wheel-scroll-amount-horizontal
   2)					; faster hscroll speed
  )
  

(provide 'nekomimi-scroll)
