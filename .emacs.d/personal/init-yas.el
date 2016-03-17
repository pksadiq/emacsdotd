;; Configurations for the powerful yasnippet

(require 'yasnippet)

;; Don't use the default snippets. Use the following instead.
(setq yas-snippet-dirs '("~/.emacs.d/personal/snippets"))

(add-hook 'term-mode-hook (lambda()
			    (yas-minor-mode -1)))

(setq yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))

;; Enable yasnippet everywhere it supports
(yas-global-mode 1)

(provide 'init-yas)
