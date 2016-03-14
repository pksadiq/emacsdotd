;; Configurations for the `ido' interactive do

(require 'ido)

;; Enable ido where ever possible
(setq ido-everywhere t)
(ido-mode 1)

;; Guess file at point on `ido-find-file'
(setq ido-use-filename-at-point 'guess)

;; Don't prompt if buffer doesn't exist
(setq ido-create-new-buffer 'always)
;; The above one seems not working, This was introduced in 23.1
(setq confirm-nonexistent-file-or-buffer nil)

(provide 'init-ido)
