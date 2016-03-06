;; Configurations related to `shell-script-mode'/Shell scripts.

;; Set space to 2
(setq sh-basic-offset 2
      sh-indentation 2)

;; Don't change '<<' to something else.
(add-hook 'sh-mode-hook (lambda () (sh-electric-here-document-mode -1)))

(provide 'init-sh)
