;; Configuring `js-mode' and `js2-mode'

;; indentation level for `js-mode'
(setq-default js-indent-level 2)

;; indentation level for `js2-mode'
(setq-default js2-basic-offset 2)

(add-hook 'js2-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)
            (electric-spacing-mode 1)))

(provide 'init-js)
