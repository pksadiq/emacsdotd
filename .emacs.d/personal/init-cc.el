;; Configurations related to `cc-mode'. That is, in java, c and c++ mode.

(add-hook 'c-mode-common-hook
    '(lambda ()
       ;; Let ';' move to the end of statement
       (local-set-key ";" 'end-statement)
       ;; Insert semi colon by C-; ; if needed
       (local-set-key (kbd "C-; ;") 'insert-semi-colon)))

(provide 'init-cc)
