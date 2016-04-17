;; Configurations related to `c-mode'

(setq-default c-basic-offset 2)

;; Multiline comment prefix
(setq c-block-comment-prefix "*")

(setq c-macro-names-with-semicolon '("G_BEGIN_DECLS"))

(defvar gnome-package nil)
(make-variable-buffer-local 'gnome-package)

(defvar gnome-parent nil)
(make-variable-buffer-local 'gnome-parent)

(defvar second-last-point nil)
(make-variable-buffer-local 'second-last-point)

(add-hook 'c-initialization-hook
          (lambda ()
            (add-hook 'post-self-insert-hook 'dwim-more nil t)
            (local-set-key (kbd "S-SPC") 'insert-space)))

(provide 'init-c)
