;; Configurations that are common to more than one Emacs mode.
;; Or configurations that doesn't fit anywhere else.

;; Don't show startup screen
(setq inhibit-startup-message t)

;; Let this file provide a feature named 'init-common'.
;; Use `require' function to load to current Emacs session.
(provide 'init-common)
