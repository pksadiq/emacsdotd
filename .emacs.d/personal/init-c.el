;; Configurations related to `c-mode'

(setq-default c-basic-offset 2)

;; Multiline comment prefix
(setq c-block-comment-prefix "*")

;; TODO: use regexp match
(setq c-macro-names-with-semicolon
      '("G_BEGIN_DECLS" "G_END_DECLS" "G_DECLARE_FINAL_TYPE"
        "G_DECLARE_DERIVABLE_TYPE" "G_DECLARE_INTERFACE" "G_DEFINE_TYPE"
        "G_DEFINE_TYPE_WITH_PRIVATE" "G_DEFINE_ABSTRACT_TYPE"
        "G_DEFINE_ABSTRACT_TYPE_WITH_PRIVATE"))

(defvar gnome-package nil)
(make-variable-buffer-local 'gnome-package)

(defvar gnome-parent nil)
(make-variable-buffer-local 'gnome-parent)

(defvar gnome-sig-defun nil)
(make-variable-buffer-local 'gnome-sig-defun)

(defvar second-last-point -1)
(make-variable-buffer-local 'second-last-point)

(defvar change-not-last nil)
(make-variable-buffer-local 'change-not-last)

(defvar next-is-command nil)
(make-variable-buffer-local 'next-is-command)

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-hook 'post-self-insert-hook 'dwim-more nil t)
            (local-set-key (kbd "S-SPC") 'insert-space)
            (local-set-key (kbd "C-,") 'insert-comma)
            (local-set-key (kbd "C-{") 'insert-brace)))

(provide 'init-c)
