;; Configurations related to `cc-mode'. That is, in java, c and c++ mode.

(add-hook 'c-mode-common-hook
    '(lambda ()
       (local-set-key ";" 'end-statement)))

(provide 'init-cc)
