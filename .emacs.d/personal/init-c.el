;; Configurations related to `c-mode'

(setq-default c-basic-offset 2)

;; Multiline comment prefix
(setq c-block-comment-prefix "*")

(setq c-macro-names-with-semicolon '("G_BEGIN_DECLS"))

(defvar gnome-package nil)
(make-variable-buffer-local 'gnome-package)

(defvar gnome-parent nil)
(make-variable-buffer-local 'gnome-parent)

(provide 'init-c)
