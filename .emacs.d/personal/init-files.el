;; Configure specific files to be opened with specific modes.

;; Let the following extensions be opened with `web-mode'
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))

(provide 'init-files)
