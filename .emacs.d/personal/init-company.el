;; Complete anything in GNU Emacs

(require 'company)

;; Enable globally
(add-hook 'after-init-hook 'global-company-mode)

;; Begin completion after 1 char
(setq company-minimum-prefix-length 1)

;; Show completion faster
(setq company-idle-delay 0.1)

;; Show enumerated completion
(setq company-show-numbers t)

(provide 'init-company)
