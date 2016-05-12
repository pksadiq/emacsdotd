;; Configuration for `web-mode'

(require 'web-mode)

(defun my-web-mode-indent-hook ()
  "Hook to set indentation"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Highlight matching tags
(setq web-mode-enable-current-element-highlight t)

;; Set grey background color for matching tags
(set-face-attribute 'web-mode-current-element-highlight-face nil :background "#333333")

(defvar last-line-empty -1)
(make-variable-buffer-local 'last-line-empty)

(add-hook 'web-mode-hook  'my-web-mode-indent-hook)

(provide 'init-web)
