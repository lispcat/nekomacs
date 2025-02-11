
;; pkg-manager x use-package integration
(pcase neko-package-manager
  ('straight
   (progn (unless (package-installed-p 'use-package)
	    (straight-use-package 'use-package))
	  (setq straight-use-package-by-default t)))
  ('package
   (progn (unless (package-installed-p 'use-package)
	    (package-install 'use-package))
	  (require 'use-package)))
  (_
   (message "Warning: neko-package-manager \"%s\" may be invalid"
	    neko-package-manager)))


(provide '40-use-package)
