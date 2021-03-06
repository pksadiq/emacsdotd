;; Configuring `js-mode' and `js2-mode'

;; indentation level for `js-mode'
(setq-default js-indent-level 2)

;; indentation level for `js2-mode'
(setq-default js2-basic-offset 2)

;; include nodejs extern defuns
(setq js2-include-node-externs t
      js2-highlight-level 3)

(eval-after-load 'js2-mode
  ;; we already have flycheck for this
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil))

(add-hook 'js2-mode-hook
          (lambda ()
            (setq mode-name "js2")
            (tern-mode 1)
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)
            (add-hook 'post-self-insert-hook 'dwim-more-js-mode nil t)
            (local-set-key ";" 'js-end-statement)
            (local-set-key (kbd "S-SPC") 'insert-space)
            (local-set-key (kbd "C-,") 'insert-comma)
            (local-set-key (kbd "C-{") 'insert-brace)
            (local-set-key (kbd "C-.") 'insert-period)
            (electric-spacing-mode 1)))

(provide 'init-js)
