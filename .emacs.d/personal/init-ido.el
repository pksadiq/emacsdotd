;; Configurations for the `ido' interactive do

(require 'ido)

;; Enable ido where ever possible
(setq ido-everywhere t)
(ido-mode 1)

;; Guess file at point on `ido-find-file'
(setq ido-use-filename-at-point 'guess)

(provide 'init-ido)
