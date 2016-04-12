;; Configuration for `web-mode'

(require 'web-mode)

(defun my-web-mode-indent-hook ()
  "Hook to set indentation"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-indent-hook)

(provide 'init-web)
