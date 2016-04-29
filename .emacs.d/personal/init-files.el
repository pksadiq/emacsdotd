;; Configure specific files to be opened with specific modes.

;; Let the following extensions be opened with `web-mode'
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.ui\\'" . web-mode))

(require 'web-mode)
(require 'flycheck)
(add-hook 'web-mode-hook (lambda ()
                           ;; check with xml-xmllint for gtk+ .ui files
                           (if (string-match-p "[.]ui$" (buffer-name))
                               (flycheck-add-mode 'xml-xmllint 'web-mode))))

(provide 'init-files)
