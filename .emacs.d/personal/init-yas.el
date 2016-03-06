;; Configurations for the powerful yasnippet

(require 'yasnippet)

;; Don't use the default snippets. Use the following instead.
(setq yas-snippet-dirs '("~/.emacs.d/personal/snippets"))

;; Enable yasnippet everywhere it supports
(yas-global-mode 1)

(provide 'init-yas)
