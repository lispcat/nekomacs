

(pcase nekomimi-package-manager
  ('straight (+require 'pkg-manager--straight))
  ('package  (+require 'pkg-manager--package))
  ('elpaca   (+require 'pkg-manager--elpaca)))

(provide '30-pkg-manager)
