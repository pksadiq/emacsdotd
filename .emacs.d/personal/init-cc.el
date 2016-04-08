;; Configurations related to `cc-mode'. That is, in java, c and c++ mode.

(add-hook 'c-mode-common-hook
    '(lambda ()
       (local-set-key ";" 'end-statement)))

;; Insert semi colon by C-; ; if needed
(add-hook 'c-mode-common-hook
    '(lambda ()
       (local-set-key (kbd "C-; ;") 'insert-semi-colon)))

(provide 'init-cc)
