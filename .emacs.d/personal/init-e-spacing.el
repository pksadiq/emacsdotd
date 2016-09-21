;; Configurations related to `electric-spacing-mode'

(require 'electric-spacing)

(setq-default electric-spacing-double-space-docs nil)

;; override electric-spacing-. defun
(defun electric-spacing-. ()
  nil)

(provide 'init-e-spacing)
