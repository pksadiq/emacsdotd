;; erc IRC client configuration

;; Hide distracting info from `erc' IRC client
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(add-hook 'erc-mode-hook
    '(lambda ()
       (local-set-key (kbd "<tab>") 'erc-get-last-nick)))

(provide 'init-erc)
