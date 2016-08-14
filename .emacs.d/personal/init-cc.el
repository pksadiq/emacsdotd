;; Configurations related to `cc-mode'. That is, in java, c and c++ mode.

(add-hook 'c-mode-common-hook
    '(lambda ()
       ;; Let ';' move to the end of statement
       (local-set-key ";" 'c-end-statement)
       ;; Insert semi colon by C-; ; if needed
       (local-set-key (kbd "C-;") 'insert-semi-colon)
       ;; enable ggtags
       (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
         (ggtags-mode 1))))

(provide 'init-cc)
